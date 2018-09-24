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
    [clojure.tools.logging :as log]
    ))

(defmulti start! identity)

(defmethod start! :dev [_]
  (let [ name "TEST-DATA"
         qnt 20
         config (platform/BINANCE (utils/edn-slurp "resources/config.edn"))
         tick-list (trade/get-ticks! platform/BINANCE "BTCUSDT" 1000)
         partitioned-ticks (partition 20 1 tick-list)] ;; TODO: review price order
                                        ; _ (visualization/build-graph! 3030 "TEST-DATA")]
    (loop [ list partitioned-ticks
            opened-position nil]

      (let [ yest-list (first list)
             today-list (second list)
             indicators (indicators/get-indicators yest-list today-list)
             current-price (:current-price indicators)
             opened! (or opened-position
                       (and (trade/open? current-price indicators)
                         (trade/open! current-price qnt config)))
             closed! (and opened-position
                       (trade/close? current-price (:price opened-position) indicators)
                       (trade/close! current-price qnt config))

             ;; for graphics
             unique-time {:time (utils/make-time-unique (:time indicators) ((comp :id last) today-list))}
             data (merge indicators unique-time {:opened-price (:price opened!)} {:closed-price (:price closed!)})]

        (log/info data)
        (Thread/sleep 1000)
        (visual/update-graph! data name)
        (recur (rest list) (when-not closed! opened!))))))

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
