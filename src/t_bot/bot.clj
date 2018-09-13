(ns t-bot.bot
  (:require
    [jutsu.core :as j]

    [t-bot.utils.market-generator :as market-generator]
    [t-bot.utils.visualization :as visualization]
    [t-bot.indicators :as indicators]

    [clj-http.client :as client]
    [throttler.core :refer [throttle-fn]]
    [clojure.edn :as edn]
    [clojure.string :as str]
    [clj-time.coerce :as time-c]
    [clj-time.format :as time-f]

    [clojure.core.async :as async :refer [go go-loop chan close! <! >!]]))

(def ^:private rate-limit 20)
(def ^:private throttle (throttle-fn identity rate-limit :second))

(defn create-url [query base-endpoint]
  (str base-endpoint query))

(defn safe-println
  "For concurrent console print."
  [& more]
  (.write *out* (str (str/join " " more) "\n")))

(defn logger [url params]
  (safe-println "Calling: " url " with parameters: " params)
  url)

(defn get-response
  ([url] (get-response url {} true))
  ([url params] (get-response url params true))
  ([url params ex?]
   (-> url
     throttle
     (logger params)
     (client/get { :as                 :auto
                   :query-params       params
                   :validate-redirects false
                   :cookie-policy      :standard
                   :throw-exceptions   ex?}))))

(defn- parse-date [in-long]
  (let [ full-f (time-f/formatters :date-hour-minute-second)
         short-f (time-f/formatters :hour-minute-second)
         date-time (time-c/from-long in-long)
         full-date (time-f/unparse full-f date-time)
         short-date (time-f/unparse short-f date-time)]
    {:short short-date :full full-date}))

(defn- parse-trade [trade]
  (let [keywordized (zipmap (map keyword (keys trade)) (vals trade))]
    (as-> keywordized t
      (update t :price (comp bigdec edn/read-string))
      (update t :qty edn/read-string)
      (assoc t :time (parse-date (:time keywordized))))))

(defn- parse-response [response]
  (let [r (as-> response r
            (:body r)
            (str/replace r #":" "")
            (edn/read-string r))]
    (map parse-trade r)))

(defn get-price
  ([symb] (get-price symb 1))
  ([symb limit]
   (let [url (create-url endpoint-trade base-endpoint)]
     (parse-response (get-response url {:symbol symb :limit limit})))))


(defmulti start! identity)

(defmethod start! :dev [_]
  (let [ price-list (market-generator/generate-prices)
         market-data (market-generator/generate-timeseries price-list)]
        ; _ (visualization/build-graph! 3030 "TEST-DATA")]
    (doseq [x market-data]
      (println x)
      (Thread/sleep 800)
      (visualization/update-graph! x))))

(defmethod start! :prod [_] (println "hello prod"))

(comment

  (j/start-jutsu! 3030 false)

  (j/graph!
    "TEST-DATA"
    [{ :x []
       :y []
       :type "scatter"}]))
