(defproject clj-ahttp "0.1.0"
  :description "async + NIO http client"
  :url "https://github.com/arohner/clj-ahttp"
  :dependencies [[com.ning/async-http-client "1.8.13"]
                 [org.clojure/core.async "0.1.319.0-6b1aca-alpha"]]
  :profiles {:dev {:dependencies [[clj-http "1.0.0"] ;; just to be able to compare responses
                                  ]}})
