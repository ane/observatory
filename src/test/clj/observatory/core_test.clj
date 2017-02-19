(ns observatory.core-test
  (:require [clojure.test :refer :all]
            [observatory.core :refer :all]
            [clj-time.core :as t]))

(def o1 (t/date-time 2017 2 17))
(def o2 (t/plus o1 (t/millis 500)))
(def o4 (t/plus o1 (t/millis 550)))
(def o3 (t/plus o2 (t/millis 1000)))

; source events
(def s1 {:id "asdf" :time o1})
(def s2 {:id "bvl2" :time o1})
(def s3 {:id "hekke" :time o1})

; target events
(def t1 {:id "asdf" :time o2})
(def t2 {:id "bvl2" :time o4})
(def t3 {:id "hekke" :time o3})

; node histories
(def n1 {:history [s1 s2 s3]})
(def n2 {:history [t1 t2 t3]})

(deftest test-by-id
  (is (= true (by-id s1 t1)))
  (is (= false (by-id s1 t2))))

(deftest test-find-pairs
  (testing "find-pairs"
    (let [corrs (find-pairs by-id n1 n2)]
      (is (= corrs [[s1 t1] [s2 t2] [s3 t3]])))))

(deftest test-temporal-ok
  (is (= (:status (check (->Temporal 600 2 0 0) n1 n2)) 'nok))
  (is (= (:status (check (->Temporal 600 2 0 2) n1 n2)) 'warn))
  (is (= (:status (check (->Temporal 600 2 2 3) n1 n2)) 'ok)))

(deftest test-temporal-time
  (is (= 1 (count (:fail (check (->Temporal 600 2 0 0) n1 n2)))))
  (is (= 2 (count (:fail (check (->Temporal 500 2 0 0) n1 n2)))))
  (is (= 3 (count (:fail (check (->Temporal 499 2 0 0) n1 n2)))))
  (is (= 1 (count (:pass (check (->Temporal 501 2 0 0) n1 n2)))))
  (is (= 2 (count (:pass (check (->Temporal 600 2 0 0) n1 n2)))))
  (is (= 3 (count (:pass (check (->Temporal 1500 3 0 0) n1 n2))))))
