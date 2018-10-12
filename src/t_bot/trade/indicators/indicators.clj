(ns t-bot.trade.indicators.indicators
  (:require
   [t-bot.trade.indicators.base :as base]
   [t-bot.trade.indicators.boll :as boll]))

(defn get-indicators
  ([tick-list]
   (let [pure-list (map :price tick-list)
         last-prices (take-last 2 pure-list)
         current-price (last pure-list)
         prev-price (first pure-list)
         sma (base/simple-moving-average pure-list)
         ema (base/exponential-moving-average pure-list)
         rsi (base/relative-strength-index pure-list)
         boll (boll/bollinger-band pure-list sma)
         macd (base/moving-averages-signals pure-list)]
     {:current-price current-price
      :prev-price prev-price
      :upper-band (:upper-band boll)
      :lower-band (:lower-band boll)
      :sma sma
      :ema ema
      :boll boll
      :rsi rsi
      :signal macd
      :time ((comp :time last) tick-list)}))
  ([tick-list history]
   (let [{:keys [boll] :as current-list-indicators} (get-indicators tick-list)
         bolls (boll/bollinger-band-signals tick-list boll history)]
     (assoc current-list-indicators :bolls bolls))))

(defn get-historical-indicators
  [history-period partitioned-ticks]
  (for [t (take history-period partitioned-ticks)]
    {:population t
     :indicators (get-indicators t)}))
