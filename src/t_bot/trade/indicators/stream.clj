(ns t-bot.trade.indicators.stream)

(defn moving-averages-crossover
  ([today-list yest-list]
    (let [ yest-sma (simple-moving-average yest-list)
           yest-ema (exponential-moving-average yest-list)
           today-sma (simple-moving-average today-list)
           today-ema (exponential-moving-average today-list)]
      (moving-averages-crossover yest-sma yest-ema today-sma today-ema)))
  ([yest-sma yest-ema today-sma today-ema]
    (let [ signal-up (and (< yest-ema yest-sma) (> today-ema today-sma))
           signal-down (and (> yest-ema yest-sma) (< today-ema today-sma))]
      (cond
        signal-up :up
        signal-down :down))))

(defn simple-moving-average-stream
  [options tick-window tick-list]
  (let [ start-index tick-window
         { :keys [input output etal]
           :or { input :price
                 output :sma
                 etal [:price :time]}} options]
    (map (fn [ech]
           (let [ tsum (reduce (fn [rr ee]
                                 (let [ltprice (:price ee)]
                                   (+ ltprice rr))) 0 ech)
                  taverage (with-precision 10 (/ tsum (count ech)))] ;; FIXME: count ech = tick-window?
             (merge
               (zipmap etal
                 (map #(% (last ech)) etal))
               { output taverage
                 :population ech})))
      (partition tick-window 1 tick-list))))

(defn exponential-moving-average-stream
  ([options tick-window tick-list]
    #_(exponential-moving-average options tick-window tick-list (simple-moving-average {} tick-window tick-list))
    (exponential-moving-average options tick-window tick-list (simple-moving-average tick-list)))
  ([options tick-window tick-list sma-list]
    (let [ k (/ 2 (+ tick-window 1))
           { :keys [input output etal]
             :or { input :price
                   output :ema
                   etal [:price :time]}} options] ;; FIXME: remove?
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
                                 ema-now (+ (with-precision 10 (* k ltprice))
                                           (with-precision 10 (* ema-last (- 1 k))))]
                            (lazy-cat rslt
                              [(merge
                                 (zipmap etal
                                   (map #(% (last (:population ech))) etal))
                                 {output ema-now})])))
              '()
              sma-list)))))

(defn join-averages-stream
  ([tick-window tick-list]
    (let [ sma-list (simple-moving-average nil tick-window tick-list)
           ema-list (exponential-moving-average nil tick-window tick-list sma-list)]
      (join-averages-stream tick-list sma-list ema-list)))
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
                 :sma (:sma sitem)
                 :ema (:ema eitem)}
               nil))
        trimmed-ticks
        sma-list
        ema-list))))

(defn bollinger-band-stream
  "From a tick-list, generates an accompanying list with upper-band and lower-band
  Upper Band: K times an N-period standard deviation above the moving average (MA + Kσ)
  Lower Band: K times an N-period standard deviation below the moving average (MA − Kσ)
  K: number of standard deviations
  N: period, or tick-window we are looking at
  Returns a list, equal in length to the tick-list, but only with slots filled,
  where preceding tick-list allows.
  ** This function assumes the latest tick is on the left**"
  ([tick-window tick-list]
    #_(bollinger-band tick-window tick-list (simple-moving-average nil tick-window tick-list))
    (bollinger-band tick-window tick-list (simple-moving-average tick-list)))
  ([tick-window tick-list sma-list]
    ;; At each step, the Standard Deviation will be: the square root of the variance (average of the squared differences from the Mean)
    (map (fn [ech]
           (let [;; get the Moving Average
                  ma (:sma ech)
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
                                        (with-precision 10 (* diff diff))))
                                 (:population ech))
                  variance (with-precision 10 (/ (reduce + sq-diff-list) (count (:population ech))))
                  standard-deviation (with-precision 10 (. Math sqrt variance))]
             { :price (:price ech)
               :prev-price ((comp :price second reverse) (:population ech))
               :time (:time ech)
               :sma ma
               :upper-band (+ ma (with-precision 10 (* 2 standard-deviation)))
               :lower-band (- ma (with-precision 10 (* 2 standard-deviation)))}))
      sma-list)))



(defn moving-averages-signals-stream
  "Takes baseline time series, along with 2 other moving averages.
  Produces a list of signals where the 2nd moving average overlaps (abouve or below) the first.
  By default, this function will produce a Simple Moving Average and an Exponential Moving Average."
  ([tick-window tick-list
    (let [ ;; sma-list (simple-moving-average nil tick-window tick-list)
           sma-list (simple-moving-average-stream nil tick-window tick-list)
           ema-list (exponential-moving-average-stream nil tick-window tick-list sma-list)]
      (moving-averages-signals-stream tick-list sma-list ema-list))])
  ([tick-list sma-list ema-list
    ;; create a list where i) tick-list ii) sma-list and iii) ema-list are overlaid
    (let [ joined-list (join-averages-stream tick-list sma-list ema-list)
           partitioned-join (partition 2 1 (remove nil? joined-list))]
      ;; find time points where ema-list (or second list) crosses over the sma-list (or 1st list)
      (map (fn [[fst snd]]
             (let [
                    ;; in the first element, has the ema crossed above the sma from the second element
                    signal-up (and (< (:ema snd) (:sma snd))
                                (> (:ema fst) (:sma fst)))
                    ;; in the first element, has the ema crossed below the sma from the second element
                    signal-down (and (> (:ema snd) (:sma snd))
                                  (< (:ema fst) (:sma fst)))
                    raw-data fst]
               ;; return either i) :up signal, ii) :down signal or iii) nothing, with just the raw data
               (assoc raw-data :signals [{ :signal (cond
                                                     signal-up :up
                                                     signal-down :down
                                                     :else :flat)
                                           :why :moving-average-crossover
                                           :arguments [fst snd]}])))
        partitioned-join))]))


(defn relative-strength-index-stream [tick-window tick-list]
  (let [ twindow (or tick-window 14)
         window-list (partition twindow 1 tick-list)]

    ;; run over the collection of populations
    (last (reductions (fn [rslt ech]
                        ;; each item will be a population of tick-window (default of 14)
                        (let [pass-one (reduce (fn [rslt ech]
                                                 (let [ fst (:last-trade-price (first ech))
                                                        snd (:last-trade-price (second ech))
                                                        up? (< fst snd)
                                                        down? (> fst snd)
                                                        sideways? (and (not up?) (not down?))]
                                                   (if (or up? down?)
                                                     (if up?
                                                       (conj rslt (assoc (first ech) :signal :up))
                                                       (conj rslt (assoc (first ech) :signal :down)))
                                                     (conj rslt (assoc (first ech) :signal :sideways)))))
                                         []
                                         (partition 2 1 (remove nil? ech)))
                               up-list (:up (group-by :signal pass-one))
                               down-list (:down (group-by :signal pass-one))
                               avg-gains (/ (apply +
                                              (map :last-trade-price up-list))
                                           tick-window)
                               avg-losses (/ (apply +
                                               (map :last-trade-price down-list))
                                            tick-window)
                               rs (if-not (= 0 avg-losses)
                                    (/ avg-gains avg-losses)
                                    0)
                               rsi (- 100 (/ 100 (+ 1 rs)))]
                          (conj rslt {:last-trade-time (:last-trade-time (first ech))
                                       :last-trade-price (:last-trade-price (first ech))
                                       :rs rs
                                       :rsi rsi})))
            []
            window-list))))

(defn sort-bollinger-band-stream [bband]
  (let [diffs (map (fn [inp]
                     (assoc inp :difference (- (:upper-band inp) (:lower-band inp))))
                (remove nil? bband))]
    (sort-by :difference diffs)))

(defn bollinger-band-signals-stream
  ([tick-window tick-list]
    (let [sma-list (simple-moving-average nil tick-window tick-list)
           bband (bollinger-band tick-window tick-list sma-list)]
      (bollinger-band-signals tick-window tick-list sma-list bband)))
  ([tick-window tick-list sma-list bband]
    (last (reductions (fn [rslt ech-list]
                        (let [
                               ;; track widest & narrowest band over the last 'n' ( 3 ) ticks
                               sorted-bands (sort-bollinger-band ech-list)
                               most-narrow (take 3 sorted-bands)
                               most-wide (take-last 3 sorted-bands)
                               partitioned-list (partition 2 1 (remove nil? ech-list))
                               upM? (up-market? 10 (remove nil? partitioned-list))
                               downM? (down-market? 10 (remove nil? partitioned-list))
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







(defn get-indicators-stream
  ([yest-tick-list today-tick-list] (get-indicators yest-tick-list today-tick-list {}))
  ([yest-tick-list today-tick-list {:keys [default-signal] :as defaults}]
    (let [ pure-yest-list (map :price yest-tick-list)
           pure-today-list (map :price today-tick-list)
           price (last pure-today-list)
           prev-price (last pure-yest-list)
           sma (simple-moving-average pure-today-list)
           ema (exponential-moving-average pure-today-list)
           boll (bollinger-band pure-today-list sma)
           macd (moving-averages-signals pure-today-list pure-yest-list)]
      { :current-price price
        :prev-price prev-price
        :upper-band (:upper-band boll)
        :lower-band (:lower-band boll)
        :sma sma
        :ema ema
        :signal (or macd default-signal)
        :time ((comp :time last) today-tick-list)})))
