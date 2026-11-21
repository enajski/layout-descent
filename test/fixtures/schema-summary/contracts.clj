(ns contracts)

(defn rule [settings] {:required-evidence settings :policy true})
(defn obligation [items] {:required-evidence items :reason "policy"})
(defn wrap [record] {:payload record :version 1})
(defn extract [record] (:required-evidence record))
(defn run []
  (let [a (rule {:unit-tests "required"}) b (obligation ["unit-tests"])]
    (extract a)
    (extract a)
    (wrap b)
    {:policy-value (extract a) :obligation-value (extract b)}))

(defn redact [x] (dissoc x :secret))
(defn after-redaction [] (:secret (redact {:secret [1] :public true})))
