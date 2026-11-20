(ns layout-descent.rt
  "Runtime of emitted layouts: the storage types and their builders. Emitted readers read the
  fields of these types directly, so a field is part of the contract with layout-descent.emit."
  (:import [java.util ArrayList Arrays Comparator HashMap TreeSet]))

(set! *warn-on-reflection* true)

;; A compressed sparse fiber tree. Depth d stores level (perm d) of the plan.
;;   dicts[d]  HashMap key -> Integer id      labels[d]  Object[] id -> key, sorted
;;   ids[d]    int[] id of each node, sorted within its parent
;;   ptrs[d]   int[] node -> first child at depth d+1, with one closing entry (absent at the last depth)
;;   leaves    double[], long[] or Object[], one per node of the last depth
(deftype Csf [^objects dicts ^objects labels ^objects ids ^objects ptrs leaves])

;; orders[i] is a HashMap root for :hash and a Csf for :csf.
(deftype Dataset [^objects orders ^long entries])

(defn- bad [code message data]
  (throw (ex-info message (assoc data :code code))))

(defn- check-key [type k path]
  (case type
    :long (if (int? k) (long k) (bad :bad-key (str "Key " (pr-str k) " at " path " is not an integer.") {:path path}))
    :keyword (if (keyword? k) k (bad :bad-key (str "Key " (pr-str k) " at " path " is not a keyword.") {:path path}))
    :string (if (string? k) k (bad :bad-key (str "Key " (pr-str k) " at " path " is not a string.") {:path path}))))

(defn wide-long-key
  "Normalize a query outside the fixed-width integer types, or return nil for a miss."
  [k]
  (when (and (integer? k) (<= Long/MIN_VALUE k Long/MAX_VALUE))
    (long k)))

(defn- check-leaf [type v path]
  (if (case type
        :double (instance? Double v)
        :long (instance? Long v)
        :object (some? v))
    v
    (bad :bad-leaf (str "Leaf " (pr-str v) " at " path " is not a " (name type) " value.") {:path path})))

(defn tuples
  "Flatten nested data into rows [k0 .. kN-1 leaf]. Throws ex-info with :code when a key, a leaf
  or a level has the wrong type."
  ^ArrayList [types leaf-type data]
  (let [n (count types)
        out (ArrayList.)]
    (letfn [(walk [node d path]
              (if (= d n)
                (.add out (object-array (conj path (check-leaf leaf-type node path))))
                (do
                  (when-not (map? node)
                    (bad :not-a-map (str "Expected a map at " path ", found " (pr-str node) ".") {:path path}))
                  (doseq [[k child] node]
                    (walk child (inc d) (conj path (check-key (nth types d) k path)))))))]
      (walk data 0 []))
    out))

(defn- build-hash [n perm ^ArrayList rows]
  (let [root (HashMap.)
        last-depth (dec (long n))]
    (doseq [^objects row rows]
      (loop [d 0
             ^HashMap node root]
        (let [k (aget row (int (nth perm d)))]
          (if (= d last-depth)
            (.put node k (aget row (int n)))
            (recur (inc d)
                   (or (.get node k)
                       (let [child (HashMap.)]
                         (.put node k child)
                         child)))))))
    root))

(defn- sorted-rows
  "Each row as int ids in storage order, closed by the index of its source row, sorted by ids."
  ^objects [n perm ^objects dicts ^ArrayList rows]
  (let [n (int n)
        m (.size rows)
        out (object-array m)]
    (dotimes [i m]
      (let [^objects row (.get rows i)
            ids (int-array (inc n))]
        (dotimes [d n]
          (aset ids d (int (.get ^HashMap (aget dicts d) (aget row (int (nth perm d)))))))
        (aset ids n (int i))
        (aset out i ids)))
    (Arrays/sort out
                 (reify Comparator
                   (compare [_ x y]
                     (let [^ints x x
                           ^ints y y]
                       (Arrays/compare x 0 n y 0 n)))))
    out))

(defn- first-differences
  "For each sorted row, the first depth where it differs from the row before it."
  ^ints [n ^objects sorted]
  (let [m (alength sorted)
        out (int-array m)]
    (loop [i 1]
      (when (< i m)
        (let [^ints x (aget sorted i)
              ^ints y (aget sorted (dec i))]
          (aset out i (int (loop [d 0] (if (= (aget x d) (aget y d)) (recur (inc d)) d)))))
        (recur (inc i))))
    out))

(defn- leaf-array [leaf-type n ^objects sorted ^ArrayList rows]
  (let [n (int n)
        m (alength sorted)
        leaf-of (fn [i] (aget ^objects (.get rows (aget ^ints (aget sorted (int i)) n)) n))]
    (case leaf-type
      :double (let [a (double-array m)] (dotimes [i m] (aset a i (double (leaf-of i)))) a)
      :long (let [a (long-array m)] (dotimes [i m] (aset a i (long (leaf-of i)))) a)
      :object (let [a (object-array m)] (dotimes [i m] (aset a i (leaf-of i))) a))))

(defn- build-csf [n perm leaf-type ^ArrayList rows]
  (let [n (int n)
        dicts (object-array n)
        labels (object-array n)]
    (dotimes [d n]
      (let [level (int (nth perm d))
            distinct-keys (TreeSet.)
            dict (HashMap.)]
        (doseq [^objects row rows] (.add distinct-keys (aget row level)))
        (let [sorted-keys (.toArray distinct-keys)]
          (dotimes [i (alength sorted-keys)] (.put dict (aget sorted-keys i) (Integer/valueOf (int i))))
          (aset labels d sorted-keys)
          (aset dicts d dict))))
    (let [sorted (sorted-rows n perm dicts rows)
          m (alength sorted)
          fd (first-differences n sorted)
          ids (object-array n)
          ptrs (object-array (max 0 (dec n)))]
      (dotimes [d n]
        (let [nodes (areduce fd i acc 0 (if (<= (aget fd i) d) (inc acc) acc))
              ids-d (int-array nodes)
              ptrs-d (when (< d (dec n)) (int-array (inc nodes)))]
          (loop [i 0
                 j 0
                 child 0]
            (if (< i m)
              (let [start? (<= (aget fd i) d)]
                (when start?
                  (aset ids-d j (aget ^ints (aget sorted i) d))
                  (when ptrs-d (aset ^ints ptrs-d j (int child))))
                (recur (inc i) (if start? (inc j) j) (if (<= (aget fd i) (inc d)) (inc child) child)))
              (when ptrs-d (aset ^ints ptrs-d nodes (int child)))))
          (aset ids d ids-d)
          (when ptrs-d (aset ptrs d ptrs-d))))
      (Csf. dicts labels ids ptrs (leaf-array leaf-type n sorted rows)))))

(defn build-dataset
  "Build every order of a descriptor {:types :leaf :orders} from nested data."
  ^Dataset [{:keys [types leaf orders]} data]
  (let [n (count types)
        rows (tuples types leaf data)]
    (Dataset. (object-array
               (map (fn [{:keys [perm format]}]
                      (case format
                        :hash (build-hash n perm rows)
                        :csf (build-csf n perm leaf rows)))
                    orders))
              (.size rows))))

(defn sum-doubles
  "Sum leaves[from, to). Emitted readers call it for a contiguous span."
  ^double [^doubles leaves ^long from ^long to]
  (loop [i from
         acc 0.0]
    (if (< i to) (recur (inc i) (+ acc (aget leaves i))) acc)))

(defn sum-longs
  "Sum leaves[from, to). Emitted readers call it for a contiguous span."
  ^long [^longs leaves ^long from ^long to]
  (loop [i from
         acc 0]
    (if (< i to) (recur (inc i) (+ acc (aget leaves i))) acc)))

(defn- hash-bytes ^long [^HashMap node ^long depth ^long n]
  (if (= depth (dec n))
    (+ 48 (* 56 (.size node)))
    (reduce (fn [^long acc child] (+ acc (hash-bytes child (inc depth) n)))
            (+ 48 (* 40 (.size node)))
            (.values node))))

(defn- csf-bytes ^long [^Csf c]
  (let [ints-bytes (fn [^objects arrays] (reduce + 0 (map #(* 4 (alength ^ints %)) arrays)))
        leaves (.-leaves c)
        leaf-bytes (* (java.lang.reflect.Array/getLength leaves) (if (instance? (Class/forName "[Ljava.lang.Object;") leaves) 24 8))]
    (long (+ (ints-bytes (.-ids c))
             (ints-bytes (.-ptrs c))
             leaf-bytes
             (reduce + 0 (map #(* 64 (alength ^objects %)) (.-labels c)))))))

(defn estimate-bytes
  "A rough heap size of a dataset from entry counts. An estimate, not a measurement."
  ^long [^Dataset ds n]
  (reduce + 0 (map (fn [order] (if (instance? Csf order) (csf-bytes order) (hash-bytes order 0 n)))
                   (.-orders ds))))
