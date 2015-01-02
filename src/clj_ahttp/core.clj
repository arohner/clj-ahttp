(ns clj-ahttp.core
  (:refer-clojure :exclude (get))
  (:require [clojure.string :as str]
            [clojure.core.async :as a]
            [clojure.tools.logging :refer (infof errorf)]
            [clj-ahttp.util :as util])
  (:import (com.ning.http.client AsyncHttpClient
                                 AsyncHttpClientConfig
                                 AsyncHttpClientConfig$Builder
                                 AsyncHandler
                                 AsyncHandler$STATE
                                 RequestBuilder
                                 HttpResponseHeaders)
           (java.nio ByteBuffer)
           (java.nio.channels Pipe
                              ReadableByteChannel)))

(defn request-method->str [request-method]
  (-> request-method
      name
      str/upper-case))

(defn build-request [{:keys [request-method url headers] :as args}]
  (assert (string? url))
  (let [builder (RequestBuilder. ^String (request-method->str request-method))]
    (.setUrl builder ^String url)
    (.build builder)))

(defn process-output [{:keys [resp-chan
                              body-chan]}])

(defn request
  "Async HTTP request. Mostly compatible w/ clj-http."
  [{:keys [uri request-method headers] :as args}]
  (let [c (AsyncHttpClient.)]
    (.executeRequest c (build-request args))))

(defn response-headers->map [^HttpResponseHeaders rh]
  (->> (.getHeaders rh)
       (map (fn [[k v]]
              [k (first v)]))
       (into {})))

(defn ^AsyncHttpClientConfig client-config []
  (let [builder (AsyncHttpClientConfig$Builder.)]
    (doto builder
      (.setIdleConnectionInPoolTimeoutInMs 1))
    (.build builder)))

(def client (AsyncHttpClient. (client-config)))

(defn request
  "Makes an async http request. Behaves similar to clj-http, except the return type is

 {:status (promise Int)
  :headers (promise {String String})
  :completed (promise Bool)
  :body java.nio.channels.ReadableByteChannel
  :abort! (fn [])}

  :status and :headers will only ever receive one value each. In case of an exception, e.g., connection refused, deref'ing any of the promises will throw the exception.

  A response map is returned immediately, potentially even before the server has sent an HTTP status.

  the key :abort! is a fn of no arguments. Call it to abort this request.

  The body channel must be (.close)'d when done. Failing to close can lead to hangs on future clj-ahttp requests."

  [{:keys [request-method uri] :as args}]
  (let [status (util/exceptional-promise)
        headers (util/exceptional-promise)
        completed (util/exceptional-promise)
        throwable (promise)
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
              :throwable throwable
              :body source
              :abort! (fn []
                        (swap! abort (constantly true)))
              :completed completed}]
    (.executeRequest ^AsyncHttpClient client (build-request args)
                     (reify AsyncHandler
                       (onThrowable [this t]
                         (println "throwable:" t)
                         (errorf throwable "error during request")

                         (deliver throwable t)
                         (deliver status t)
                         (deliver headers t)
                         (deliver completed t))
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
                         (.close sink)
                         (deliver completed true))))
    resp))

(defn drain-resp
  "Convert to a normal clj-http-style response"
  [resp]
  (let [len (-> resp :headers a/<!! (clojure.core/get "Content-Length") (#(Long/parseLong %)))
        ^ReadableByteChannel body-chan (-> resp :body)
        buf (ByteBuffer/allocate (* 2 len))]
    (loop []
      (let [ret (.read body-chan buf)]
        (if (.hasRemaining buf)
          (if (= ret -1)
            (do
              (.close body-chan)
              (.flip buf)
              buf)
            (recur))
          (throw (java.nio.BufferOverflowException "out of space")))))))

(defn get [uri & [opts]]
  (request (merge opts
                  {:request-method :get
                   :uri uri})))
