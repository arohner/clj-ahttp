(ns clj-ahttp.core-test
  (:require [clojure.test :refer :all]
            [clj-ahttp.core :as ahttp]
            [nio.core :as nio]))

(deftest client-options
  (is (ahttp/new-client {:connection-pooling? true
                         :idle-timeout 1000
                         :connection-timeout 1000
                         :follow-redirects? false})))

(deftest basic-functionality
  (let [resp (clj-ahttp.core/get "http://google.com")]
    (is (integer? @(:status resp)))
    (is (map? @(:headers resp)))
    (is (string? (slurp (:body resp))))
    (.close (:body resp))
    (is (realized? (:completed resp)))
    (is @(:completed resp))))

(deftest throwable-bogus-server
  (let [resp (clj-ahttp.core/get "http://bogus.server")]
    (is @(:throwable resp))
    (is (nil? @(:status resp)))
    (is (nil? @(:headers resp)))
    (is @(:completed resp))))

(deftest throwable-bogus-port
  (let [resp (clj-ahttp.core/get "http://localhost:9999")]
    (is @(:throwable resp))
    (is (nil? @(:status resp)))
    (is (nil? @(:headers resp)))
    (is @(:completed resp))))
