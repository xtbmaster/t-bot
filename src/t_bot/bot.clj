(ns t-bot.bot
  (:require
    [t-bot.utils.market-generator :as market-generator]
    [t-bot.utils.visualization :as visual]
    [t-bot.indicators :as indicators]
    [t-bot.utils.utils :as utils]
    [t-bot.trade :as trade]
    [t-bot.platforms :as platform]

    [clj-http.client :as client]
    [throttler.core :refer [throttle-fn]]
    [clojure.edn :as edn]
    [clojure.string :as str]
    [clojure.tools.logging :as log]

    [clojure.core.async :as async :refer [go go-loop chan close! <! >!]]))

(defn- parse-trade [trade]
  (let [keywordized (zipmap (map keyword (keys trade)) (vals trade))]
    (as-> keywordized t
      (update t :price (comp bigdec edn/read-string))
      (update t :qty edn/read-string)
      (assoc t :time (utils/parse-date (:time keywordized))))))

(defn- parse-response [response]
    (map parse-trade (:body response)))

;; TODO: pass config
(defn get-ticks!
  ([platform symb] (get-ticks! platform symb 1))
  ([platform symb limit]
    (let [ endpoints (platform (edn/read-string (clojure.core/slurp "resources/endpoints.edn")))
           url (create-url (:ticks endpoints) (:base endpoints))]
      (parse-response (utils/get-response url {:symbol symb :limit limit})))))

(defmulti start! identity)

(defmethod start! :dev [_]
  (let [ name "TEST-DATA"
         qnt 20
         config (platform/BINANCE (edn/read-string (clojure.core/slurp "resources/config.edn")))
         trading-fee (:trading-fee config)
         tick-list (get-ticks! platform/BINANCE "BTCUSDT" 1000)
         partitioned-ticks (partition 20 1 tick-list)] ;; TODO: review price order
                                        ; _ (visualization/build-graph! 3030 "TEST-DATA")]
    (loop [ list partitioned-ticks
            yest-list (first partitioned-ticks)
            today-list (second partitioned-ticks)
            opened-position nil]
      (let [ indicators (indicators/get-indicators yest-list today-list)
             id {:id ((comp :id last) today-list)}
             unique-time {:time (utils/make-time-unique (:time indicators) (:id id))}
             current-price (:current-price indicators)
             opened! (or opened-position
                       (when (trade/open? current-price indicators)
                         (trade/open! current-price qnt trading-fee unique-time)))
             closed! (when opened-position
                       (when (trade/close? current-price (:price opened-position) qnt indicators)
                         (trade/close! current-price qnt trading-fee unique-time)))
             data (merge indicators id unique-time
                    (or closed!))]
        (log/info data)
        (Thread/sleep 1000)
        (visual/update-graph! data name)
        (recur (rest list) today-list (second (rest list)) (:open-price open-price))))))

(defmethod start! :prod [_] (println "hello prod"))

(comment
  (visual/start-jutsu! 3031)
  (visual/graph! "TEST-DATA")
  (start! :dev))


;; TODO: logging to file
;; TODO: console control
;; TODO: connection lose handling
;; TODO: request timeout
;; TODO: RSI
;; TODO: tests :)
;; TODO: standard deviation comparison
