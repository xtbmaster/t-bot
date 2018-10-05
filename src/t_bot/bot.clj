(ns t-bot.bot
  (:require
    [t-bot.auxiliary.market-generator :as market-generator]
    [t-bot.auxiliary.visualization :as visual]
    [t-bot.trade.indicators :as indicators]
    [t-bot.auxiliary.utils :as utils]
    [t-bot.trade.trade :as trade]
    [t-bot.trade.utils :as trade-utils]
    [t-bot.trade.platforms :as platforms]

    [clojure.tools.logging :as log]
    ))

(defmulti start! identity)

(defmethod start! :dev [_]
  (let [ name "TEST-DATA"
         config (utils/edn-slurp "resources/config.edn")
         pair (-> platforms/BINANCE :pairs :btcusdt) ;; TODO: move to args
         platform-name (:name platforms/BINANCE) ;; TODO: move to args
         tick-list (trade-utils/get-ticks! platform-name pair 1000)
         partitioned-ticks (partition 20 1 tick-list)] ;; TODO: review price order
                                        ; _ (visualization/build-graph! 3030 NAME)]
    (loop [ [tick-list & remaining-ticks] partitioned-ticks
            opens { :population []
                    :last-open nil
                    :last-close nil
                    :total-qnt 0
                    :total-value 0}
            signal nil]

      (let [ indicators (-> (indicators/get-indicators tick-list)
                          (assoc-in [:signal :prev] signal))
             trade-params { :limit-factor (-> config :trading :common :order-limit-factor)
                            :opens-limit (-> config :trading :common :open-positions-limit)
                            :indicators indicators
                            :fee (-> config :trading platform-name :trading-fee)
                            :qnt (-> config :trading :common :trade-qnt)
                            :pair pair
                            :platform-name platform-name}
             portfolio (trade/update-positions! opens trade-params (:trading config))

             ;; for graphics
             unique-time {:time (utils/make-time-unique (:time indicators) ((comp :id last) tick-list))}
             data (merge indicators
                    unique-time
                    {:open-price (-> portfolio :last-open :price)}
                    {:close-price (-> portfolio :last-close :price)})]

        ;; logging
        (when (or (zero? (count (:population portfolio)))
                (not= (count (:population portfolio)) (count (:population opens))))
          (log/info (str
                      "TIME: " (:time unique-time) " | "
                      " PRICE: " (:current-price indicators) " | "
                      " TOTAL OPENS: " (count (:population portfolio)))))

        (Thread/sleep 800)
        (let [ data-for-graph (cond-> data
                                (= (:last-open opens) (:last-open portfolio)) (dissoc data :open-price)
                                (= (:last-close opens) (:last-close portfolio)) (dissoc data :close-price)
                                (= signal (-> indicators :signal :current)) (dissoc data :signal))]
          (visual/update-graph! data-for-graph name))
        (recur remaining-ticks portfolio (-> indicators :signal :current))))))

(defmethod start! :prod [_] (println "hello prod"))

(comment
  (visual/start-jutsu! 3031)
  (visual/graph! "TEST-DATA")
  (start! :dev))

;; TODO: trading on bearish market
;; TODO: console control
;; TODO: connection lose handling
;; TODO: request timeout
;; TODO: RSI graphic
;; TODO: boll signals
;; TODO: standard deviation comparison
;; TODO: std deviation limit constraint (flat cases)

;; TODO: logging to file
;; TODO: tests :)
;; TODO; freefall
;; TODO: documentation
;; TODO: trade statistics
