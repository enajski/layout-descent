(ns layout-descent.emit
  "Deterministic code emission: one plan in, one Clojure namespace out. The emitted readers are
  straight-line loops for one pattern on one order; nothing in them depends on runtime layout data."
  (:require [clojure.string :as str]
            [layout-descent.plan :as plan]))

(defn- nest [k s]
  (str/replace s "\n" (str "\n" (apply str (repeat k " ")))))

(defn- key-expr [type level]
  (let [k (str "k" level)]
    (if (= :long type)
      (str "(if (int? " k ") (long " k ") (rt/wide-long-key " k "))")
      k)))

(defn- mode [{:keys [bind fold]}]
  (cond
    fold fold
    (every? #{:key} bind) :point
    :else :reduce))

(defn- combine
  "The form that folds one leaf into the result."
  [{:keys [mode leaf scans]} leaf-expr]
  (case mode
    :point leaf-expr
    :reduce (str "(let [next (f (aget cell 0) " (str/join " " (map #(str "s" %) scans)) " " leaf-expr ")]\n"
                 "  (if (reduced? next)\n"
                 "    (do (aset cell 0 @next) (aset cell 1 true))\n"
                 "    (aset cell 0 next)))")
    :sum (str "(aset cell 0 (+ (aget cell 0) (" (if (= :double leaf) "double" "long") " " leaf-expr ")))")
    :count "(aset cell 0 (inc (aget cell 0)))"))

(defn- add-to-cell [expr]
  (str "(aset cell 0 (+ (aget cell 0) " expr "))"))

;; A scan is a statement loop that writes a one-element cell. A nested `loop` that returned its
;; accumulator would sit in expression position, where Clojure wraps it in a closure and boxes it.
;; A custom reducer also uses a second slot to signal early termination to every enclosing loop.

(defn- hash-level [{:keys [n perm bind types mode] :as ctx} d]
  (let [level (nth perm d)
        last? (= d (dec n))
        node (str "^java.util.HashMap n" d)
        child (if last? "leaf" (str "n" (inc d)))
        inner #(if last? (combine ctx "leaf") (hash-level ctx (inc d)))]
    (cond
      (= :key (nth bind level))
      (str "(let [" child " (.get " node " " (key-expr (nth types level) level) ")]\n"
           "  (when-not (nil? " child ")\n"
           "    " (nest 4 (inner)) "))")

      (and last? (= :count mode))
      (add-to-cell (str "(.size " node ")"))

      :else
      (str "(let [it" d " (.iterator (.entrySet " node "))]\n"
           "  (while " (if (= :reduce mode)
                         (str "(and (nil? (aget cell 1)) (.hasNext it" d "))")
                         (str "(.hasNext it" d ")")) "\n"
           "    (let [^java.util.Map$Entry e" d " (.next it" d ")\n"
           (when (= :reduce mode) (str "          s" level " (.getKey e" d ")\n"))
           "          " child " (.getValue e" d ")]\n"
           "      " (nest 6 (inner)) ")))"))))

(defn- span-end
  "One end of the span that the subtree under depth d covers at the last depth."
  [{:keys [n]} d end]
  (reduce (fn [expr depth] (str "(aget ptrs" depth " " expr ")")) (str end d) (range d (dec n))))

(defn- csf-level [{:keys [n perm bind types mode leaf] :as ctx} d]
  (let [level (nth perm d)
        last? (= d (dec n))
        p (str "p" d)
        only-scans-below? (every? #(= :scan (nth bind %)) (subvec perm d))
        inner #(if last?
                 (combine ctx (str "(aget leaves " p ")"))
                 (str "(let [a" (inc d) " (aget ptrs" d " " p ")\n"
                      "      b" (inc d) " (aget ptrs" d " (inc " p "))]\n"
                      "  " (nest 2 (csf-level ctx (inc d))) ")"))]
    (cond
      (and (= :key (nth bind level)) (zero? d))
      (str "(let [id (.get dict0 " (key-expr (nth types level) level) ")]\n"
           "  (when-not (nil? id)\n"
           "    (let [p0 (int id)]\n"
           "      " (nest 6 (inner)) ")))")

      (= :key (nth bind level))
      (str "(let [id (.get dict" d " " (key-expr (nth types level) level) ")]\n"
           "  (when-not (nil? id)\n"
           "    (let [" p " (java.util.Arrays/binarySearch ids" d " (int a" d ") (int b" d ") (int id))]\n"
           "      (when-not (neg? " p ")\n"
           "        " (nest 8 (inner)) "))))")

      ;; The leaves under one node are contiguous, so trailing scans need no loop over nodes.
      (and only-scans-below? (= :count mode))
      (add-to-cell (str "(- " (span-end ctx d "b") " " (span-end ctx d "a") ")"))

      (and only-scans-below? (= :sum mode))
      (add-to-cell (str "(rt/sum-" (name leaf) "s leaves " (span-end ctx d "a") " " (span-end ctx d "b") ")"))

      (= :reduce mode)
      (str "(loop [" p " a" d "]\n"
           "  (when (and (nil? (aget cell 1)) (< " p " b" d "))\n"
           "    (let [s" level " (aget labels" d " (aget ids" d " " p "))]\n"
           "      " (nest 6 (inner)) ")\n"
           "    (recur (inc " p "))))")

      :else
      (str "(loop [" p " a" d "]\n"
           "  (when (< " p " b" d ")\n"
           "    " (nest 4 (inner)) "\n"
           "    (recur (inc " p "))))"))))

(defn- uses? [body sym]
  (some? (re-find (re-pattern (str "(?<![\\w-])" sym "(?![\\w-])")) body)))

(defn- csf-bindings [{:keys [n leaf]} idx body]
  (let [body (if (uses? body "b0") (str body " ids0") body)
        per-depth (fn [stem hint field]
                    (for [d (range n) :when (uses? body (str stem d))]
                      [(str hint " " stem d) (str "(aget ^objects (." field " c) " d ")")]))]
    (concat
     [["^layout_descent.rt.Csf c" (str "(aget ^objects (.-orders ds) " idx ")")]]
     (per-depth "dict" "^java.util.HashMap" "-dicts")
     (per-depth "labels" "^objects" "-labels")
     (per-depth "ids" "^ints" "-ids")
     (per-depth "ptrs" "^ints" "-ptrs")
     (when (uses? body "leaves")
       [[(str ({:double "^doubles" :long "^longs" :object "^objects"} leaf) " leaves") "(.-leaves c)"]])
     (when (uses? body "a0") [["a0" "0"]])
     (when (uses? body "b0") [["b0" "(alength ids0)"]]))))

(defn signature
  "How a caller invokes the emitted function of one pattern: its name, arguments and result."
  [plan pattern]
  (let [level-ids (mapv :id (:levels plan))
        bind (:bind pattern)
        m (mode pattern)
        bound (filterv #(= :key (nth bind %)) (range (count bind)))
        scans (filterv #(= :scan (nth bind %)) (range (count bind)))]
    {:pattern (:id pattern)
     :fn (name (:id pattern))
     :args (vec (concat ["ds"] (when (= :reduce m) ["f" "init"]) (map #(name (nth level-ids %)) bound)))
     :returns (case m
                :point "the leaf, or nil when the path is absent"
                :reduce (str "the unordered reduction; f is called as (f acc "
                             (str/join " " (map #(name (nth level-ids %)) scans)) " leaf). "
                             "Use an order-independent reducer; (reduced value) stops the scan and returns value")
                :sum "the sum of the selected leaves; the order of addition is not specified"
                :count "the number of selected leaves")}))

(defn- pattern-source [plan pattern]
  (let [idx (get (:serve plan) (:id pattern))
        {:keys [perm format]} (nth (:orders plan) idx)
        bind (:bind pattern)
        n (count bind)
        m (mode pattern)
        leaf (:type (:leaf plan))
        ctx {:n n
             :perm perm
             :bind bind
             :types (mapv :type (:levels plan))
             :mode m
             :leaf leaf
             :scans (filterv #(= :scan (nth bind %)) (range n))}
        body (if (= :hash format) (hash-level ctx 0) (csf-level ctx 0))
        bindings (concat
                  (if (= :hash format)
                    [["^java.util.HashMap n0" (str "(aget ^objects (.-orders ds) " idx ")")]]
                    (csf-bindings ctx idx body))
                  (case m
                    :point nil
                    :reduce [["cell" "(object-array 2)"]]
                    :sum [["cell" (if (= :double leaf) "(double-array 1)" "(long-array 1)")]]
                    :count [["cell" "(long-array 1)"]]))
        sig (signature plan pattern)
        params (concat ["ds"]
                       (when (= :reduce m) ["f" "init"])
                       (for [level (range n) :when (= :key (nth bind level))] (str "k" level)))]
    (str "(defn " (:fn sig) "\n"
         "  \"Pattern " (:id pattern) " " bind ", served by order " idx " " perm " " format ".\n"
         "  Returns " (:returns sig) ".\"\n"
         "  {:arglists '([" (str/join " " (:args sig)) "])}\n"
         "  [^layout_descent.rt.Dataset " (str/join " " params) "]\n"
         "  (let [" (str/join "\n        " (map (fn [[k v]] (str k " " v)) bindings)) "]\n"
         (when (= :reduce m) "    (aset cell 0 init)\n")
         "    " (nest 4 body)
         (when-not (= :point m) "\n    (aget cell 0)")
         "))")))

(defn ns-symbol
  "The namespace an emitted plan defines when the caller gives no other."
  [plan]
  (or (get-in plan [:emit :ns]) (symbol (str "layout-descent.generated." (:name plan)))))

(defn source
  "The complete source text of the namespace that implements a normal plan. Equal plans give equal
  text. opts may name the namespace with :ns."
  ([plan] (source plan nil))
  ([plan opts]
   (let [hash (plan/plan-hash plan)
         descriptor (array-map :types (mapv :type (:levels plan))
                               :leaf (:type (:leaf plan))
                               :orders (mapv (fn [{:keys [perm format]}]
                                               (array-map :perm perm :format format))
                                             (:orders plan)))]
     (str ";; GENERATED by layout-descent from plan " hash ". Do not edit: change the plan and emit again.\n"
          "(ns " (or (:ns opts) (ns-symbol plan)) "\n"
          "  \"Layout " (:name plan) ", compiled from plan " hash ".\"\n"
          "  (:require [layout-descent.rt :as rt]))\n\n"
          "(set! *warn-on-reflection* true)\n\n"
          "(def plan-hash \"" hash "\")\n\n"
          "(def descriptor\n  '" (pr-str descriptor) ")\n\n"
          "(defn build\n"
          "  \"Compile a nested map into the dataset that the functions of this namespace read.\"\n"
          "  [data]\n"
          "  (rt/build-dataset descriptor data))\n\n"
          (str/join "\n\n" (map #(pattern-source plan %) (:patterns plan)))
          "\n"))))
