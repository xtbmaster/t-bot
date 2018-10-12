(ns t-bot.bot
  (:require
    [t-bot.auxiliary.market-generator :as market-generator]
    [t-bot.auxiliary.visualization :as visual]
    [t-bot.trade.indicators.indicators :as indicators]
    [t-bot.auxiliary.utils :as utils]
    [t-bot.trade.trade :as trade]
    [t-bot.trade.utils :as trade-utils]
    [t-bot.trade.platforms :as platforms]

    [clojure.tools.logging :as log]))

(defmulti start! identity)

(defmethod start! :dev [_]
  (let [name "TEST-DATA"
        config (utils/edn-slurp "resources/config.edn")
        pair (-> platforms/BINANCE :pairs :btcusdt) ;; TODO: move to args
        platform-name (:name platforms/BINANCE) ;; TODO: move to args
        tick-list (trade-utils/get-ticks! platform-name pair 1000)
        partitioned-ticks (partition 20 1 tick-list)
        history-limit (-> config :trading :common :tick-history-period)
        historical-ticks (indicators/get-historical-indicators history-limit partitioned-ticks)]
    ;; TODO: review price order
                                        ; _ (visualization/build-graph! 3030 NAME)]
    (loop [[tick-list & remaining-ticks] (drop history-limit partitioned-ticks)
           hist historical-ticks
           opens trade-utils/opens-template]

      (let [indicators (indicators/get-indicators tick-list hist)
            trade-params (trade-utils/pack-trade-params config platform-name pair indicators)
            portfolio (trade/update-positions! opens trade-params (:trading config))
            history (-> historical-ticks
                        (conj {:population tick-list :indicators indicators})
                        (utils/trim-vector history-limit))
            unique-time {:time (utils/make-time-unique (:time indicators) ((comp :id last) tick-list))}]

        ;; logging
        (when (or (zero? (count (:population portfolio)))
                  (not= (count (:population portfolio)) (count (:population opens))))
          (log/info (str
                     "TIME: " (:time unique-time) " | "
                     " PRICE: " (:current-price indicators) " | "
                     " TOTAL OPENS: " (count (:population portfolio)))))

        (Thread/sleep 800)
        (visual/update-graph! name portfolio indicators unique-time)
        (recur remaining-ticks history portfolio)))))

(defmethod start! :prod [_] (println "hello prod"))

(comment
  (visual/start-jutsu! 3031)
  (visual/graph! "TEST-DATA")
  (start! :dev))

;; TODO: console control
;; TODO: connection lose handling
;; TODO: request timeout
;; TODO: RSI graphic
;; TODO: boll signals
;; TODO: standard deviation comparison ?? boll signals
;; TODO: std deviation limit constraint (flat cases)

;; TODO: logging to file
;; TODO: tests :)
;; TODO: freefall
;; TODO: documentation
;; TODO: trade statistics
