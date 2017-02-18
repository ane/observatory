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

(defn event-diff
  [e1 e2]
  (t/interval (:time e1) (:time e2)))

(defn event-within?
  [win]
  (fn [pair] (<= (let [as-ms (t/in-millis (:diff pair))]
                   as-ms) win)))

(defn to-diff
  [[p1 p2]]
  {:diff (event-diff p1 p2)
   :src p1
   :dst p2})

(deftype Temporal [win min max]
  Object
  (toString [this] (format "Temporal %sms min=%d max=%d" win min max))
  Checker
  (check [this n1 n2]
    (let [pairs (find-pairs by-id n1 n2)]
      (->> pairs
           (map to-diff)
           (filter (event-within? win))))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
