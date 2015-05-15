(ns clj-ahttp.core
  (:refer-clojure :exclude (get))
  (:require [clojure.string :as str]
            [clojure.core.async :as a]
            [clj-ahttp.util :as util])
  (:import java.io.InputStream
           (com.ning.http.client AsyncHttpClient
                                 AsyncHttpClientConfig
                                 AsyncHttpClientConfig$Builder
                                 AsyncHandler
                                 AsyncHandler$STATE
                                 RequestBuilder
                                 HttpResponseHeaders
                                 generators.InputStreamBodyGenerator)
           (java.nio ByteBuffer)
           (java.nio.channels Pipe
                              ReadableByteChannel)))

(defn request-method->str [request-method]
  (-> request-method
      name
      str/upper-case))

(def end-padding (.getBytes "\r\n"))

(defn read-nio-with-chunking
  "Implement HTTP chunking ourselves, for the http clients that don't do it natively"
  [src-chan chunk dest-buf eof-count]
  {:post [(integer? %)]}
  (.clear chunk)
  (let [put-chunk (fn [src-buf dest-buf]
                    (.put dest-buf (-> (Integer/toHexString (.limit src-buf)) .getBytes))
                    (.put dest-buf end-padding)
                    (.put dest-buf chunk)
                    (.put dest-buf end-padding))
        read (.read src-chan chunk)]
    (if (pos? read)
      (.limit chunk read)
      (.limit chunk 0))
    (.rewind chunk)
    (cond
      (> read 0) (do
                   (put-chunk chunk dest-buf)
                   read)
      (= read 0) 0
      (= read -1) (do
                    (swap! eof-count inc)
                    (cond
                      (= @eof-count 1) (do
                                         ;; put an empty chunk
                                         (put-chunk chunk dest-buf)
                                         (.position dest-buf))
                      :else -1)))))

(defn nio-byte-body-generator
  "Given an NIO channel, return a ning.http.client.BodyGenerator"
  [^ReadableByteChannel chan & [{:keys [nio-chunking?]
                                 :or {nio-chunking? true}}]]
  (let [chunk-size 1024
        chunk (ByteBuffer/allocate chunk-size)
        eof-count (atom 0)]
    (reify com.ning.http.client.BodyGenerator
      (createBody [this]
        (reify com.ning.http.client.Body
          (close [this]
            (.close chan))
          (getContentLength [this]
            -1)
          (read [this dest-buf]
            (if nio-chunking?
              (read-nio-with-chunking chan chunk dest-buf eof-count)
              (.read chan dest-buf))))))))

(defprotocol ToBody
  (to-body
    "Use the input for the request body"
    [x builder]))

(extend-protocol ToBody
  (Class/forName "[B")
  (to-body [byte-arr builder]
    (.setBody builder ^bytes byte-arr))
  String
  (to-body [str ^RequestBuilder builder]
    (.setBody builder ^String str))
  InputStream
  (to-body [is builder]
    (.setBody builder (InputStreamBodyGenerator. is)))
  ReadableByteChannel
  (to-body [rbc builder]
           (.setBody builder ^BodyGenerator (nio-byte-body-generator rbc))))

(defn build-request [{:keys [request-method url body] :as args}]
  (assert (string? url))
  (let [builder (RequestBuilder. ^String (request-method->str request-method))]
    (.setUrl builder ^String url)
    (when body
      (to-body body builder))
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
      (.setAllowPoolingConnections builder connection-pooling?))
    (when idle-timeout
      (.setIdleConnectionInPoolTimeoutInMs builder idle-timeout))
    (when connection-timeout
      (.setConnectTimeout builder connection-timeout))
    (when (not (nil? follow-redirects?))
      (.setFollowRedirect builder follow-redirects?))
    (let [config (.build builder)]
      (AsyncHttpClient. config))))

(def default-client (new-client))

(defn request
  "Makes an async http request. Behaves similar to clj-http, except the return type is

 {:status (promise (t/Option Int)
  :headers (promise (t/Option {String String})
  :completed (promise Bool)
  :throwable (promise (t/Option Throwable))
  :body java.nio.channels.ReadableByteChannel
  :abort! (fn [])}

  In case of an exception, e.g., connection refused, the :throwable
  promise will be delivered with the exception. All other promises
  will be delivered with nil.

  The :completed promise is delivered when the last byte of :body has been transferred.

  A response map is returned immediately, potentially even before the server has sent an HTTP status.

  the key :abort! is a fn of no arguments. Call it to abort this request.

  The body channel must be (.close)'d when done. Failing to close can lead to hangs on future clj-ahttp requests.


  Options:

  :request-method - keyword, same as clj-http
  :uri - string, same as clj-http
  :client - optional, a clj-ahttp client, created by #'new-client.
  :body - input data to send, String, InputStream or byte[]

"

  [{:keys [request-method uri client body] :as args
    :or {client default-client}}]
  (let [status (promise)
        headers (promise)
        completed (promise)
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
                         (deliver status nil)
                         (deliver headers nil)
                         (deliver completed true))
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
