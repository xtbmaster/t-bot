(ns t-bot.trade.indicators.base
  (:require
   [t-bot.auxiliary.utils :as utils]))

(defn simple-moving-average
  [tick-list]
  (let [last-tick (last tick-list)
        n (count tick-list)
        tsum (reduce + tick-list)]
    (with-precision 10 (/ tsum n))))

(defn exponential-moving-average [tick-list]
  (let [a (/ 2 (+ (count tick-list) 1))
        ema-list (reduce (fn [res ct]
                           (conj
                             res
                             (+ (with-precision 10 (* a ct))
                               (with-precision 10 (* (- 1 a) (peek res))))))
                   [(first tick-list)]
                   (rest tick-list))]
    (last ema-list)))

(defn mean [tick-list]
  (let [sum (apply + tick-list)
        count (count tick-list)]
    (if (pos? count)
      (with-precision 10 (/ sum count))
      0)))

(defn standard-deviation
  [tick-list]
  (let [avg (mean tick-list)
        squares (for [x tick-list]
                  (let [x-avg (- x avg)]
                    (with-precision 10 (* x-avg x-avg))))
        total (count tick-list)]
    (with-precision 10
      (Math/sqrt
        (with-precision 10 (/ (apply + squares) (- total 1)))))))

(defn moving-averages-signals
  [tick-list]
  (let [sma (simple-moving-average tick-list)
        ema (exponential-moving-average tick-list)]
    (cond
      (> ema sma) :up
      (< ema sma) :down
      :else :sideways)))

(defn relative-strength-index
  [tick-list]
  (let [n-ticks (count tick-list)
        price-move (map (fn [[f s]]
                          (cond
                            (< f s) {:gain s}
                            (> f s) {:loss s}
                            :else {:sideways s})) (partition 2 1 tick-list))
        gain-sum (utils/get-sum :gain price-move)
        loss-sum (utils/get-sum :loss price-move)
        avg-gains (with-precision 10 (/ gain-sum (- n-ticks 1)))
        avg-losses (with-precision 10 (/ loss-sum (- n-ticks 1)))
        rs (if (or (zero? avg-losses) (zero? avg-gains))
             0
             (with-precision 10 (/ avg-gains avg-losses)))
        rsi (- 100 (with-precision 10 (/ 100 (+ 1 rs))))]
    rsi))

