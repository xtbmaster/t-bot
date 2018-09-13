(ns t-bot.utils
  (:require
    [clj-http.client :as client]
    [clojure.string :as str]
    [throttler.core :refer [throttle-fn]]))

(defn- create-url [query base-endpoint]
  (str base-endpoint query))

(defn- safe-println
  "For concurrent console print."
  [& more]
  (.write *out* (str (str/join " " more) "\n")))

(defn- logger [url]
  (safe-println "Calling: " url)
  url)

(defn get-response
  ([url] (get-response url {} true))
  ([url params] (get-response url params true))
  ([url params ex?
     (-> url throttle logger
       (client/get { :as                 :auto
                     :query-params       params
                     :validate-redirects false
                     :cookie-policy      :standard
                     :throw-exceptions   ex?}))]))
