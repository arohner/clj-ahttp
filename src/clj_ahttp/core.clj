(ns clj-ahttp.core
  (:refer-clojure :exclude (get))
  (:require [clojure.string :as str]
            [clojure.core.async :as a]
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

(defn build-request [{:keys [request-method url] :as args}]
  (assert (string? url))
  (let [builder (RequestBuilder. ^String (request-method->str request-method))]
    (.setUrl builder ^String url)
    (.build builder)))

(defn process-output [{:keys [resp-chan
                              body-chan]}])

(defn request
  "Async HTTP request. Mostly compatible w/ clj-http."
  [{:keys [url request-method headers] :as args}]
  (let [c (AsyncHttpClient.)]
    (.executeRequest c (build-request args))))

(defn response-headers->map [^HttpResponseHeaders rh]
  (->> (.getHeaders rh)
       (map (fn [[k v]]
              [k (first v)]))
       (into {})))

(defn ^AsyncHttpClient new-client
  "Returns a new Client. options:

 connection-timeout: (in ms) Throw if a connection can't be established in N ms
 connection-pooling? - boolean. Pool and reuse connections. (a.k.a http keepalive)
 idle-timeout: (in ms). When using connection pooling, close after timeout
 follow-redirects? - boolean.

 When using multiple clients, .close'ing them is a good idea.
 "
  [& [{:keys [connection-pooling?
              idle-timeout
              connection-timeout
              follow-redirects?]}]]
  (let [builder (AsyncHttpClientConfig$Builder.)]
    (when (not (nil? connection-pooling?))
      (.setAllowPoolingConnection builder connection-pooling?))
    (when idle-timeout
      (.setIdleConnectionInPoolTimeoutInMs builder idle-timeout))
    (when connection-timeout
      (.setConnectionTimeoutInMs builder connection-timeout))
    (when (not (nil? follow-redirects?))
      (.setFollowRedirects builder follow-redirects?))
    (let [config (.build builder)]
      (AsyncHttpClient. config))))

(def default-client (new-client))

(defn request
  "Makes an async http request. Behaves similar to clj-http, except the return type is

 {:status (promise Int)
  :headers (promise {String String})
  :completed (promise Bool)
  :body java.nio.channels.ReadableByteChannel
  :abort! (fn [])}

  In case of an exception, e.g., connection refused, deref'ing any of the promises will throw the exception.

  A response map is returned immediately, potentially even before the server has sent an HTTP status.

  the key :abort! is a fn of no arguments. Call it to abort this request.

  The body channel must be (.close)'d when done. Failing to close can lead to hangs on future clj-ahttp requests.


  Options:

  :request-method - keyword, same as clj-http
  :uri - string, same as clj-http
  :client - optional, a clj-ahttp client, created by #'new-client."

  [{:keys [request-method uri client] :as args
    :or {client default-client}}]
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

(defn get [url & [opts]]
  (request (merge opts
                  {:request-method :get
                   :url url})))
