(ns clj-ahttp.core
  (:refer-clojure :exclude (get))
  (:require [clojure.string :as str]
            [clojure.core.async :as a])
  (:import (com.ning.http.client AsyncHttpClient
                                 AsyncHandler
                                 AsyncHandler$STATE
                                 RequestBuilder)
           (java.nio.channels Pipe)))

(defn request-method->str [request-method]
  (-> request-method
      name
      str/upper-case))

(defn build-request [{:keys [request-method uri headers] :as args}]
  (assert (string? uri))
  (let [builder (RequestBuilder. ^String (request-method->str request-method))]
    (.setUrl builder ^String uri)
    (.build builder)))

(defn process-output [{:keys [resp-chan
                              body-chan]}])

(defn request
  "Async HTTP request. Mostly compatible w/ clj-http.

options
:output - allowed options are :future, :async
"
  [{:keys [uri request-method headers] :as args}]
  (let [c (AsyncHttpClient.)]
    (.executeRequest c (build-request args))))

(defn response-headers->map [^HttpResponseHeaders rh]
  (->> (.getHeaders rh)
       (map (fn [[k v]]
              [k (first v)]))
       (into {})))

(defn request
  "Makes an async http request. Behaves like clj-http, except the return type is

 {:status (promise Int)
  :headers (promise {})
  :body java.nio.channels.ReadableByteChannel}

A response is returned immediately, potentially even before the server has sent a status.

The response map also contains a key, :abort!, a fn of no arguments. Call it to abort processing.
 "
  [{:keys [] :as args}]
  (let [c (AsyncHttpClient.)
        status (promise)
        headers (promise)
        pipe (Pipe/open)
        source (.source pipe)
        sink (.sink pipe)
        abort (atom false)
        return-state (fn []
                       (if @abort
                         AsyncHandler$STATE/ABORT
                         AsyncHandler$STATE/CONTINUE))
        resp {:status status
              :headers headers
              :body source
              :abort! (fn []
                        (swap! abort (constantly true)))}]
    (.executeRequest c (build-request args) (reify AsyncHandler
                                              (onThrowable [this throwable]
                                                (throw throwable))
                                              (onStatusReceived [this s]
                                                (deliver status (.getStatusCode s))
                                                (return-state))
                                              (onHeadersReceived [this h]
                                                (deliver headers (response-headers->map h))
                                                (return-state))
                                              (onBodyPartReceived [this body-part]
                                                (let [buf (.getBodyByteBuffer body-part)
                                                      buf-len (.remaining buf)
                                                      written (.write sink buf)]
                                                  (assert (= written buf-len)))
                                                (return-state))
                                              (onCompleted [this]
                                                (.close sink))))
    resp))

(defn get [uri & [opts]]
  (request (merge opts
                  {:request-method :get
                   :uri uri})))
