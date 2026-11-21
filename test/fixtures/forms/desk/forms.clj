(ns desk.forms)

(def defaults {:mode :fast :retries 3})

(defn guarded [order]
  (if-let [line (:line order)] (:qty line) (:fallback order)))

(defn routed [ticket]
  (cond (:urgent? ticket) (:owner ticket)
        (:stale? ticket) (:queue ticket)
        :else (:id ticket)))

(defn enriched [ticket user]
  (cond-> ticket (:admin? user) (assoc :owner (:name user))))

(defn safe [request]
  (try (:body request)
       (catch Exception e (ex-message e))
       (finally (:trace request))))

(defn totals [lines]
  (reduce (fn [sum line] (+ sum (:amount line))) 0 lines))

(defn open-ids [tickets]
  (for [t tickets :when (:open? t)] (:id t)))

(defn paid [orders]
  (->> orders (filter :paid?) (map :total)))

(defn head [orders] (:id (first orders)))

(defn summary [order] (select-keys order [:id :total]))

(defn configured [] (:mode defaults))

(defn merged [a b] (:x (merge a b)))

(defn looped [xs] (loop [ys xs] (recur (rest ys))))

(defn qualified [{:slot/keys [id] :keys [slot/coord] :as slot}]
  {:id id :coord coord :kind (:slot/kind slot)})

(def ^:dynamic *options* {:root true})
(def ^:dynamic *mirror* nil)

(defn read-bound-option [] (:outer *options*))

(defn scoped-binding []
  (let [result (binding [*options* {:outer true}]
                 {:nested (binding [*options* {:inner true} *mirror* *options*]
                            [(:inner *options*) (:outer *mirror*)])
                  :restored (:outer desk.forms/*options*)
                  :helper (read-bound-option)
                  :shadowed (let [*options* {:local true}] (:local *options*))})]
    [result (:root *options*)]))
