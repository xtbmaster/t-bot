(ns t-bot.indicators)

(defn simple-moving-average
  [options tick-window tick-list]
  (let [ start-index tick-window
         { :keys [input output etal]
           :or { input :last
                 output :last-average
                 etal [:last :last-time]}} options]
    (map (fn [ech]
           (let [ tsum (reduce (fn [rr ee]
                                  (let [ltprice (:price ee)]
                                    (+ ltprice rr))) 0 ech)
                  taverage (/ tsum (count ech))]
               (merge
                 (zipmap etal
                   (map #(% (last ech)) etal))
                 { output taverage
                   :population ech})))
        (partition tick-window 1 tick-list))))

(defn average [tick-list]
  (let [n (count tick-list)]
    (/ (reduce + tick-list) n)))

(defn mean [coll]
  (let [sum (apply + coll)
         count (count coll)]
    (if (pos? count)
      (/ sum count)
      0)))

(defn standard-deviation [coll]
  (let [ avg (mean coll)
         squares (for [x coll]
                   (let [x-avg (- x avg)]
                     (* x-avg x-avg)))
         total (count coll)]
    (-> (/ (apply + squares)
          (- total 1))
      (Math/sqrt))))


(defn bollinger-band_
  "From a tick-list, generates an accompanying list with upper-band and lower-band
  Upper Band: K times an N-period standard deviation above the moving average (MA + Kσ)
  Lower Band: K times an N-period standard deviation below the moving average (MA − Kσ)
  K: number of standard deviations
  N: period, or tick-window we are looking at
  Returns a list, equal in length to the tick-list, but only with slots filled,
  where preceding tick-list allows.
  ** This function assumes the latest tick is on the left**"

  ([tick-window tick-list]
   (bollinger-band tick-window tick-list (simple-moving-average nil tick-window tick-list)))

  ([tick-window tick-list sma-list]

   ;; At each step, the Standard Deviation will be: the square root of the variance (average of the squared differences from the Mean)
   (reduce (fn [rslt ech]

             (let [;; get the Moving Average
                   ma (:last-trade-price-average ech)

                   ;; work out the mean
                   mean (/ (reduce (fn [rslt ech]
                                     (+ (:last-trade-price ech)
                                        rslt))
                                   0
                                   (:population ech))
                           (count (:population ech)))

                   ;; Then for each number: subtract the mean and square the result (the squared difference)
                   sq-diff-list (map (fn [ech]
                                       (let [diff (- mean (:last-trade-price ech))]
                                         (* diff diff)))
                                     (:population ech))

                   variance (/ (reduce + sq-diff-list) (count (:population ech)))
                   standard-deviation (. Math sqrt variance)]

               (lazy-cat rslt
                         [{:last-trade-price (:last-trade-price ech)
                           :last-trade-time (:last-trade-time ech)
                           :upper-band (+ ma (* 2 standard-deviation))
                           :lower-band (- ma (* 2 standard-deviation))}])))
           '()
     sma-list)))


