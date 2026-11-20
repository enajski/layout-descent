;; A fixture for the facts extractor: scans whose function reads the scanned value. It is read,
;; never loaded.
(ns sample.scans)

;; The column sum of a map that is nested by row only: every row is visited for one key.
(defn col-sum
  ^double [rows col]
  (reduce-kv (fn [sum _ row]
               (if-let [value (get row col)]
                 (+ (double sum) (double value))
                 sum))
             0.0
             rows))

(defn grand-total [rows]
  (reduce-kv (fn [sum _ row]
               (reduce-kv (fn [sum _ value] (+ sum value)) sum row))
             0.0
             rows))

(defn short-col-sum [rows col]
  (reduce-kv #(+ %1 (get %3 col 0.0)) 0.0 rows))

(defn hinted [rows col]
  (reduce-kv (fn [^double sum _ ^java.util.Map row] (+ sum (double (get row col)))) 0.0 rows))

(defn deep [db tenant feature]
  (reduce-kv (fn step [sum _ entity] (+ sum (get-in entity [:features feature] 0.0)))
             0.0
             (get-in db [:scores tenant])))

(defn two-reads [rows a b]
  (reduce-kv (fn [sum _ row] (+ sum (get row a) (get row b))) 0.0 rows))

(defn shadowed [rows col]
  (reduce-kv (fn [sum _ row]
               (let [row (meta row)]
                 (+ sum (get row col))))
             0.0
             rows))

(defn destructured [rows col]
  (reduce-kv (fn [sum _ {:keys [weights]}] (+ sum (get weights col))) 0.0 rows))

(defn key-read [rows other]
  (reduce-kv (fn [sum k _] (+ sum (get other k))) 0.0 rows))

(defn rebound [rows col]
  (reduce-kv #(let [[%3] %2] (+ %1 (get %3 col))) 0.0 rows))

(defn rebuilt [rows col]
  (reduce-kv (fn [out k row] (assoc out k (dissoc row col))) {} rows))
