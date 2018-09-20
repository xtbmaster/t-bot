(ns t-bot.bot
  (:require
    [jutsu.core :as j]

    [t-bot.utils.market-generator :as market-generator]
    [t-bot.utils.visualization :as visual]
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
  (let [ full-f (time-f/formatters :date-hour-minute-second-ms)
         short-f (time-f/formatters :hour-minute-second-ms)
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
    (map parse-trade (:body response)))

;; TODO: pass config
(defn get-ticks!
  ([platform symb] (get-ticks! platform symb 1))
  ([platform symb limit]
   (let [ endpoints (platform (edn/read-string (clojure.core/slurp "resources/endpoints.edn")))
          url (create-url (:ticks endpoints) (:base endpoints))]
     (parse-response (get-response url {:symbol symb :limit limit})))))

;; { :price price
;;   :prev-price prev-price
;;   :upper-band (:upper-band boll)
;;   :lower-band (:lower-band boll)
;;   :price-average sma
;;   :price-exponential ema
;;   :signal macd
;;   :time ((comp :time last) tick-list)}



;; TODO: UNIFY
(defn- open?
  [{:keys [current-price prev-price upper-band lower-band signal]}]
  (when-not (identical? :down signal)
    (and (<= current-price lower-band) (> current-price prev-price))))

(defn- close?
  [{:keys [current-price prev-price upper-band lower-band signal]}]
  (when-not (identical? :up signal)
    (and (>= current-price upper-band) (< current-price prev-price))))

(defmulti start! identity)

(defmethod start! :dev [_]
  (let [ name "TEST-DATA"
         tick-list (get-ticks! BINANCE "ADABTC" 1000)
         partitioned-ticks (partition 20 1 tick-list)] ;; TODO: review price order
                                        ; _ (visualization/build-graph! 3030 "TEST-DATA")]
    (doseq [ x partitioned-ticks]
      (let [ indicators (indicators/get-indicators x)
             open-price (:open-price (when (open? indicators) (:current-price indicators)))
             close-price (:close-price (when (close? indicators) (:current-price indicators)))
             data (merge indicators open-price close-price)]
        (log/info indicators)
        (Thread/sleep 1000)
        (visual/update-graph! data name)))))

(defmethod start! :prod [_] (println "hello prod"))

(comment
  (visual/start-jutsu! 3031)
  (visual/graph! "TEST-DATA")
  (start! :dev))

;; FIXME: trades of the same time

(map (get-ticks! BINANCE "ADABTC" 20))


;; TODO: logging to file
;; TODO: console control
;; TODO: connection lose handling
;; TODO: request timeout
;; TODO: RSI
;; TODO: tests :)
