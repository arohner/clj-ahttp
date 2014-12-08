(ns clj-ahttp.util-test
  (:require [clojure.test :refer :all]
            [clj-ahttp.util :as util]))

(deftest exceptional-promise-works
  (testing "normal usage"
    (let [p (util/exceptional-promise)]
      (is (not (realized? p)))
      (deliver p :foo)
      (is (realized? p))
      (is (= :foo @p))))
  (testing "exceptions are thrown on deref"
    (let [p (util/exceptional-promise)
          e (try
              (/ 1 0)
              (catch Exception e
                e))]
      (deliver p e)
      (is (realized? p))
      (try
        @p
        (is false)
        (catch Exception thrown
          (is (= thrown e)))))))
