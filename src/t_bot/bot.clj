(ns t-bot.bot
  (:require
    [jutsu.core :as j]

    [t-bot.utils.market-generator :as market-generator]
    [t-bot.utils.visualization :as visualization]
    [t-bot.indicators :as indicators]
    [t-bot.utils.utils :as utils]

    [clj-http.client :as client]
    [throttler.core :refer [throttle-fn]]
    [clojure.edn :as edn]
    [clojure.string :as str]
    [clj-time.coerce :as time-c]
    [clj-time.format :as time-f]

    [clojure.tools.logging :as log]

    [clojure.core.async :as async :refer [go go-loop chan close! <! >!]]))

(def ^:const BINANCE :binance)


(def ^:private rate-limit 20)
(def ^:private throttle (throttle-fn identity rate-limit :second))

(def ^:private config (edn/read-string (clojure.core/slurp "resources/config.edn")))

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

(defn get-ticks!
  ([platform symb] (get-ticks! platform symb 1))
  ([platform symb limit]
   (let [ endpoints (platform (edn/read-string (clojure.core/slurp "resources/endpoints.edn")))
          url (create-url (:ticks endpoints) (:base endpoints))]
     (parse-response (get-response url {:symbol symb :limit limit})))))


(defn- open?
  [price {:keys [prev-price] {:keys [lower-band]} :boll}]
  (and (<= price lower-band) (> price prev-price)))

;; TODO: UNIFY
(defn- close?
  [price {:keys [prev-price] {:keys [upper-band]} :boll}]
  (and (>= price upper-band) (< price prev-price)))

(defmulti start! identity)

(defmethod start! :dev [_]
  (let [ name "TEST-DATA"
         ;price-list (market-generator/generate-prices)
         ; time-series (market-generator/generate-timeseries price-list)
         tick-list (get-ticks! BINANCE "ADABTC" 1000)
         sma-list (indicators/simple-moving-average nil 20 tick-list)
         ema-list (indicators/exponential-moving-average nil 20 tick-list sma-list)
         boll (indicators/bollinger-band 20 tick-list sma-list)
         masd (indicators/moving-averages-signals tick-list sma-list ema-list)]
                                        ; _ (visualization/build-graph! 3030 "TEST-DATA")]
    (doseq [{:keys [price time price-average upper-band lower-band] :as x}  boll]
      (let [ indicators {:boll x}
             open-price (when (open? price indicators)
                          price)
             close-price (when (close? price indicators)
                           price)
             data { :price price
                    :open-price open-price
                    :close-price close-price
                    :upper-band upper-band
                    :lower-band lower-band
                    :time time}]
        (log/info data)
        (Thread/sleep 1000)
        (visualization/update-graph! data name)))))

(defmethod start! :prod [_] (println "hello prod"))

(comment

  (j/start-jutsu! 3031 false)

  (j/graph!
    "TEST-DATA"
    [ { :x []
        :y []
        :type "line"
        :name "price"}
      { :x []
        :y []
        :type "line"
        :name "upper-band"
        :line { :dash "dashdot"}}
      { :x []
        :y []
        :type "line"
        :name "lower-band"
        :line { :dash "dashdot"}}
      { :x []
        :y []
        :type "scatter"
        :mode "markers"
        :name "open"
        :marker {:line { :width 3}
                  :opacity 0.5
                  :size 12
                  :symbol "circle-open"}}
      { :x []
        :y []
        :type "scatter"
        :mode "markers"
        :name "close"
        :marker { :line { :width 3}
                  :opacity 0.5
                  :size 12
                  :symbol "circle-open"}}]))

;; TODO: telegram integration
;; TODO: logging to file
;; TODO: console control
;; TODO: connection lose handling
;; TODO: request timeout
;; TODO: MACD or RSI
