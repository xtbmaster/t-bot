(ns t-bot.bot
  (:require
    [t-bot.utils.market-generator :as market-generator]
    [t-bot.utils.visualization :as visual]
    [t-bot.indicators :as indicators]
    [t-bot.utils.utils :as utils]
    [t-bot.trade :as trade]
    [t-bot.platforms :as platform]

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
    (loop [ [tick-list & remaining] partitioned-ticks
            opened-positions {}
            signal nil]

      (let [ indicators (indicators/get-indicators tick-list)
             current-price (:current-price indicators)
             opens (-> opened-positions
                     (trade/try-to-open! current-price qnt indicators config)
                     (trade/try-to-close! current-price qnt indicators config))
             ;; for graphics
             unique-time {:time (utils/make-time-unique (:time indicators) ((comp :id last) tick-list))}
             data (merge indicators
                    unique-time
                    {:value (:value closed!)}
                    {:open-price (:price opened!)}
                    {:close-price (:price closed!)})]

        (log/info (dissoc data :price-exponential :price-average))
        (Thread/sleep 800)
        (let [ data-for-graph (cond-> data
                                opened-position (dissoc data :open-price)
                                (= signal (:signal indicators)) (dissoc data :signal))]
          (visual/update-graph! data-for-graph name))
        (recur remaining (when-not closed! opened!) (:signal indicators))))))

(defmethod start! :prod [_] (println "hello prod"))

(comment
  (visual/start-jutsu! 3031)
  (visual/graph! "TEST-DATA")
  (start! :dev))


;; TODO: console control
;; TODO: connection lose handling
;; TODO: request timeout
;; TODO: RSI
;; TODO: standard deviation comparison
;; TODO: relative lower-upper-band price difference in %
;; TODO: std deviation limit constraint (flat cases)
;; TODO: open several positions

;; TODO: logging to file
;; TODO: tests :)
;; TODO; freefall
