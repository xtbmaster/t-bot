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
    (loop [ [tick-list & remaining-ticks] partitioned-ticks
            opens { :population []
                    :last-open nil
                    :last-close nil
                    :total-qnt 0
                    :total-value 0}
            signal nil]

      (let [ indicators (indicators/get-indicators tick-list)
             portfolio (trade/update-positions! opens indicators config)
             ;; for graphics
             unique-time {:time (utils/make-time-unique (:time indicators) ((comp :id last) tick-list))}
             data (merge indicators
                    unique-time
                    {:open-price (get-in portfolio [:last-open :price])}
                    {:close-price (get-in portfolio [:last-close :price])})]

        (log/info (str
                    "TIME: " unique-time
                    "\nPRICE: " (:current-price indicators)
                    "\nTOTAL OPENS " (count (:population portfolio))))
        (Thread/sleep 800)
        (let [ data-for-graph (cond-> data
                                (= (:last-open opens) (:last-open portfolio) (dissoc data :open-price))
                                (= (:last-close opens) (:last-close portfolio) (dissoc data :close-price))
                                (= signal (:signal indicators)) (dissoc data :signal))]
          (visual/update-graph! data-for-graph name))
        (recur remaining-ticks portfolio (:signal indicators))))))

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

;; TODO: logging to file
;; TODO: tests :)
;; TODO; freefall
;; TODO: documentation
;; TODO: trade statistics
