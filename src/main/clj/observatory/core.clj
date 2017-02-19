(ns observatory.core
  (:gen-class)
  (:require [clj-time.core :as t]))

(defn by-id
  [e1 e2]
  (= (:id e1) (:id e2)))

(defn find-pairs
  [pairfn n1 n2]
  (for [h1 (:history n1)
        h2 (:history n2)
        :when (pairfn h1 h2)]
    [h1 h2]))

(defprotocol Checker
  (check [this n1 n2]))

(deftype Temporal [win ok warn fail]
  Object
  (toString [this] (format "Temporal %sms ok=%s warn=%s nok=%s" win ok warn fail))
  Checker
  (check [this n1 n2]
    (let [[passing failing] (->> (find-pairs by-id n1 n2) ; get correlations
                                 (map (fn [[p1 p2]] (t/interval (:time p1) (:time p2))))
                                 (map t/in-millis)
                                 (split-with (fn [interval] (>= win interval))))]                  ; split into [(ok) (bad)]
      {:pass passing
       :fail failing
       :status (cond (<= fail (count failing)) 'nok
                     (<= warn (count failing)) 'warn
                     (<= ok   (count passing)) 'ok)})))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
