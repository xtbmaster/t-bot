(ns t-bot.indicators)

(defn simple-moving-average
  [options tick-window tick-list]
  (let [ start-index tick-window
         { :keys [input output etal]
           :or { input :price
                 output :price-average
                 etal [:price :time]}} options]
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

(defn exponential-moving-average
  ([options tick-window tick-list]
    (exponential-moving-average options tick-window tick-list (simple-moving-average {} tick-window tick-list)))
  ([options tick-window tick-list sma-list]
    ;; 1. calculate 'k'
    ;; k = 2 / N + 1
    ;; N = number of days
    (let [ k (/ 2 (+ tick-window 1))
           { :keys [input output etal]
             :or { input :price
                   output :price-exponential
                   etal [:price :time]}} options]
      ;; 2. get the simple-moving-average for a given tick - 1
      (last (reductions (fn [rslt ech]
                          ;; 3. calculate the EMA ( for the first tick, EMA(yesterday) = MA(yesterday) )
                          (let [ ;; price(today)
                                 ltprice (input ech)
                                 ;; EMA(yesterday)
                                 ema-last (if (output (last rslt))
                                            (output (last rslt))
                                            (input ech))
                                 ;; ** EMA now = price(today) * k + EMA(yesterday) * (1 - k)
                                 ema-now (+ (* k ltprice)
                                           (* ema-last (- 1 k)))]
                            (lazy-cat rslt
                              [(merge
                                 (zipmap etal
                                   (map #(% (last (:population ech))) etal))
                                 {output ema-now})])))
              '()
              sma-list)))))

(defn join-averages
  ([tick-window tick-list]
    (let [ sma-list (simple-moving-average nil tick-window tick-list)
           ema-list (exponential-moving-average nil tick-window tick-list sma-list)]
      (join-averages tick-list sma-list ema-list)))
  ([tick-list sma-list ema-list]
    (let [trimmed-ticks (drop-while #(not (= (:time %)
                                            (:time (first sma-list))))
                          tick-list)]
      (map (fn [titem sitem eitem]
             (if (and (and (not (nil? (:time sitem)))
                        (not (nil? (:time eitem))))
                   (= (:time titem) (:time sitem) (:time eitem)))
               { :time (:time titem)
                 :price (:price titem)
                 :price-average (:price-average sitem)
                 :price-exponential (:price-exponential eitem)}
               nil))
        trimmed-ticks
        sma-list
        ema-list))))

(defn bollinger-band
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
    (map (fn [ech]
           (let [;; get the Moving Average
                  ma (:price-average ech)
                  ;; work out the mean
                  mean (/ (reduce (fn [rslt ech]
                                    (+ (:price ech)
                                      rslt))
                            0
                            (:population ech))
                         (count (:population ech)))
                  ;; Then for each number: subtract the mean and square the result (the squared difference)
                  sq-diff-list (map (fn [ech]
                                      (let [diff (- mean (:price ech))]
                                        (* diff diff)))
                                 (:population ech))
                  variance (/ (reduce + sq-diff-list) (count (:population ech)))
                  standard-deviation (. Math sqrt variance)]
             { :price (:price ech)
               :prev-price ((comp :price second reverse) (:population ech))
               :time (:time ech)
               :price-average ma
               :upper-band (+ ma (* 2 standard-deviation))
               :lower-band (- ma (* 2 standard-deviation))}))
      sma-list)))

(defn moving-averages-signals
  "Takes baseline time series, along with 2 other moving averages.
  Produces a list of signals where the 2nd moving average overlaps (abouve or below) the first.
  By default, this function will produce a Simple Moving Average and an Exponential Moving Average."
  ([tick-window tick-list]
    (let [ sma-list (simple-moving-average nil tick-window tick-list)
           ema-list (exponential-moving-average nil tick-window tick-list sma-list)]
      (moving-averages-signals tick-list sma-list ema-list)))
  ([tick-list sma-list ema-list]
    ;; create a list where i) tick-list ii) sma-list and iii) ema-list are overlaid
    (let [ joined-list (join-averages tick-list sma-list ema-list)
           partitioned-join (partition 2 1 (remove nil? joined-list))]
      ;; find time points where ema-list (or second list) crosses over the sma-list (or 1st list)
      (map (fn [[fst snd]]
             (let [
                    ;; in the first element, has the ema crossed abouve the sma from the second element
                    signal-up (and (< (:price-exponential snd) (:price-average snd))
                                (> (:price-exponential fst) (:price-average fst)))
                    ;; in the first element, has the ema crossed below the sma from the second element
                    signal-down (and (> (:price-exponential snd) (:price-average snd))
                                  (< (:price-exponential fst) (:price-average fst)))
                    raw-data fst]
               ;; return either i) :up signal, ii) :down signal or iii) nothing, with just the raw data
               (if signal-up
                 (assoc raw-data :signals [{ :signal :up
                                             :why :moving-average-crossover
                                             :arguments [fst snd]}])
                 (if signal-down
                   (assoc raw-data :signals [{ :signal :down
                                               :why :moving-average-crossover
                                               :arguments [fst snd]}])
                   raw-data))))
        partitioned-join))))
