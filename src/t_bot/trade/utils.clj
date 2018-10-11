(ns t-bot.trade.utils
  (:require
    [t-bot.auxiliary.utils :as utils]
    [clojure.edn :as edn]))

(defn adjusted-limit
  "Calculates a new order-limit band according to `ease-factor` (%).
  Function is based on boll indicator and uses `sma` and upper or
  lower `band` depending on whether we need to change a sell or
  buy limit."
  [sma band ease-factor]
  (let [factor (/ ease-factor 100)
        dif (- sma band)
        eased-dif (with-precision 10 (* factor dif))
        new-limit (+ dif eased-dif)]
    new-limit))

(defn calculate-value
  ([price qnt] (calculate-value price qnt 0))
  ([price qnt fee-percentage]
   (let [revenue (with-precision 10 (* price qnt))
         fee-amount (with-precision 10 (/ (* fee-percentage revenue) 100))]
     (- revenue fee-amount))))

(defn- parse-trade [trade]
  (let [keywordized (zipmap (map keyword (keys trade)) (vals trade))]
    (as-> keywordized t
      (update t :price (comp bigdec edn/read-string))
      (update t :qty edn/read-string)
      (assoc t :time (utils/parse-date (:time keywordized))))))

(defn- parse-response [response]
  (map parse-trade (:body response)))

;; TODO: unify with other platforms
(defn get-ticks!
  ([platform symb] (get-ticks! platform symb 1))
  ([platform symb limit]
   (let [ endpoints (platform (utils/edn-slurp "resources/endpoints.edn"))
          url (utils/create-url (:ticks endpoints) (:base endpoints))]
     (parse-response (utils/get-response url {:symbol symb :limit limit})))))

(defn- market-direction
  "Can be used to find out whether there was a solid
  bullish or bearish trend in the last `period`. `f`
  receives '>' to check for a down trend and '<' for an up
  trend."
  [f period tick-list]
  (every? #(f (first %) (second %))
    (take period tick-list)))

(defn up-market? [period tick-list]
  (market-direction < period tick-list)) ;; TODO: check operator

(defn down-market? [period tick-list]
  (market-direction > period tick-list)) ;; TODO: same here

(defn pack-trade-params [config platform-name pair indicators]
  {:limit-factor (-> config :trading :common :order-limit-factor)
   :opens-limit (-> config :trading :common :open-positions-limit)
   :indicators indicators
   :fee (-> config :trading platform-name :trading-fee)
   :qnt (-> config :trading :common :trade-qnt)
   :pair pair
   :platform-name platform-name})

(def opens-template
  {:population []
   :last-open nil
   :last-close nil
   :total-qnt 0
   :total-value 0})
