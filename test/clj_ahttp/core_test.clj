(ns clj-ahttp.core-test
  (:require [clojure.test :refer :all]
            [clj-ahttp.core :as ahttp]
            [nio.core :as nio]))

(deftest basic-functionality
  (let [resp (clj-ahttp.core/get "http://google.com")]
    (is (integer? @(:status resp)))
    (is (map? @(:headers resp)))
    (is (string? (slurp (:body resp))))
    (.close (:body resp))
    (is (realized? (:completed resp)))
    (is @(:completed resp))))

(deftest throwable
  (let [resp (clj-ahttp.core/get "http://bogus.server")]
    (is (realized? (:throwable resp)))
    (is (thrown? Exception @(:status resp)))
    (is (thrown? Exception @(:headers resp)))
    (is @(:throwable resp))))
