(ns clj-ahttp.util)

(defn exceptional-promise
  "Same as clojure.core/promise, but if the value stored in the
  promise is an instance of j.l.Exception, will throw the exception on
  deref"
  []
  (let [d (java.util.concurrent.CountDownLatch. 1)
        v (atom d)]
    (reify
      clojure.lang.IDeref
      (deref [_]
        (.await d)
        (if (instance? Exception @v)
          (throw @v)
          @v))
      clojure.lang.IBlockingDeref
      (deref
        [_ timeout-ms timeout-val]
        (if (.await d timeout-ms java.util.concurrent.TimeUnit/MILLISECONDS)
          (if (instance? Exception @v)
                              (throw @v)
                              @v)
          timeout-val))
      clojure.lang.IPending
      (isRealized [this]
        (zero? (.getCount d)))
      clojure.lang.IFn
      (invoke
        [this x]
        (when (and (pos? (.getCount d))
                   (compare-and-set! v d x))
          (.countDown d)
          this)))))
