(ns clj-ahttp.core-test
  (:require [clojure.test :refer :all]
            [clj-ahttp.core :as ahttp]))

(deftest basic-functionality
  (let [resp (clj-ahttp.core/get "http://google.com")]
    (is (integer? @(:status resp)))
    (is (map? (inspect @(:headers resp))))))
