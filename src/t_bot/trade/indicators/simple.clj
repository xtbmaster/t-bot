(ns t-bot.trade.indicators.simple
  (:require
    [t-bot.auxiliary.utils :as utils]))

(defn simple-moving-average
  [tick-list]
  (let [ last-tick (last tick-list)
         n (count tick-list)
         tsum (reduce + tick-list)]
    (with-precision 10 (/ tsum n))))

(defn exponential-moving-average [tick-list]
  (let [ a (/ 2 (+ (count tick-list) 1))
         ema-list (reduce (fn [res ct]
                            (conj
                              res
                              (+ (with-precision 10 (* a ct))
                                (with-precision 10 (* (- 1 a) (peek res))))))
                    [(first tick-list)]
                    (rest tick-list))]
    (last ema-list)))

(defn mean [tick-list]
  (let [ sum (apply + tick-list)
         count (count tick-list)]
    (if (pos? count)
      (with-precision 10 (/ sum count))
      0)))

(defn standard-deviation
  [tick-list]
  (let [ avg (mean tick-list)
         squares (for [x tick-list]
                   (let [x-avg (- x avg)]
                     (with-precision 10 (* x-avg x-avg))))
         total (count tick-list)]
    (with-precision 10
      (Math/sqrt
        (with-precision 10 (/ (apply + squares) (- total 1)))))))

(defn bollinger-band
  ([tick-list] (bollinger-band tick-list (simple-moving-average tick-list)))
  ([tick-list sma]
    (let [ stdev (standard-deviation tick-list)
           upper-band (+ sma (with-precision 10 (* stdev 2)))
           lower-band (- sma (with-precision 10 (* stdev 2)))]
      { :upper-band upper-band
        :lower-band lower-band})))

(defn moving-averages-signals
  [tick-list]
  (let [ sma (simple-moving-average tick-list)
         ema (exponential-moving-average tick-list)]
    (cond
      (> ema sma) :up
      (< ema sma) :down
      :else :sideways)))

;; TODO: check this one
(defn relative-strength-index
  [tick-list]
  (let [ n-ticks (count tick-list)
         price-move (map (fn [[f s]]
                           (cond
                             (< f s) {:gain s}
                             (> f s) {:loss s}
                             :else {:sideways s})) (partition 2 1 tick-list))
         gain-sum (utils/sum-keys :gain price-move)
         loss-sum (utils/sum-keys :loss price-move)
         avg-gains (with-precision 10 (/ gain-sum (- n-ticks 1)))
         avg-losses (with-precision 10 (/ loss-sum (- n-ticks 1)))
         rs (if-not (zero? avg-losses)
              (with-precision 10 (/ avg-gains avg-losses))
              0)
         rsi (- 100 (with-precision 10 (/ 100 (+ 1 rs))))]
    rsi))

(defn sort-bollinger-band [bband]
  (let [diffs (map #(assoc % :difference (- (:upper-band %) (:lower-band %)))
                (remove nil? bband))]
    (sort-by :difference diffs)))




(defn bollinger-band-signals
  ([tick-list]
    (let [ sma (simple-moving-average tick-list)
           bband (bollinger-band tick-list sma)]
      (bollinger-band-signals tick-list sma bband)))
  ([tick-list sma bband]
    (last (reductions (fn [rslt ech-list]
                        (let [
                               ;; track widest & narrowest band over the last 'n' ( 3 ) ticks
                               sorted-bands (sort-bollinger-band ech-list)
                               most-narrow (take 3 sorted-bands)
                               most-wide (take-last 3 sorted-bands)
                               partitioned-list (partition 2 1 (remove nil? ech-list))
                               upM? (trade-utils/up-market? 10 (remove nil? partitioned-list))
                               downM? (trade-utils/down-market? 10 (remove nil? partitioned-list))
                               side-market? (and (not upM?)
                                              (not downM?))
                               ;; find last 3 peaks and valleys
                               peaks-valleys (find-peaks-valleys nil (remove nil? ech-list))
                               peaks (:peak (group-by :signal peaks-valleys))
                               valleys (:valley (group-by :signal peaks-valleys))]
                          (if (empty? (remove nil? ech-list))
                            (conj rslt nil)
                            (if (or upM? downM?)
                              ;; A.
                              (calculate-strategy-a rslt ech-list most-narrow upM? peaks valleys)
                              ;; B.
                              (calculate-strategy-b rslt ech-list most-wide peaks valleys)))
                          (conj rslt (first ech-list))))
            []
            (partition tick-window 1 bband)))))




(defn get-indicators [tick-list]
  (let [ pure-list (map :price tick-list)
         last-prices (take-last 2 pure-list)
         current-price (last pure-list)
         prev-price (first pure-list)
         sma (simple-moving-average pure-list)
         ema (exponential-moving-average pure-list)
         rsi (relative-strength-index pure-list)
         boll (bollinger-band pure-list sma)
         macd (moving-averages-signals pure-list)]
    { :current-price current-price
      :prev-price prev-price
      :upper-band (:upper-band boll)
      :lower-band (:lower-band boll)
      :sma sma
      :ema ema
      :rsi rsi
      :signal { :prev nil
                :current macd}
      :time ((comp :time last) tick-list)}))
