(ns t-bot.trade.indicators.boll
  (:require
   [t-bot.trade.indicators.base :as base]
   [t-bot.trade.utils :as trade-utils]))

(defn bollinger-band
  ([tick-list] (bollinger-band tick-list (base/simple-moving-average tick-list)))
  ([tick-list sma]
   (let [stdev (base/standard-deviation tick-list)
         upper-band (+ sma (with-precision 10 (* stdev 2)))
         lower-band (- sma (with-precision 10 (* stdev 2)))]
     {:upper-band upper-band
      :lower-band lower-band})))


(defn sort-bollinger-band [bband]
  (let [diffs (map #(assoc % :difference (- (:upper-band %) (:lower-band %))) bband)]
    (sort-by :difference diffs)))

;; TODO check this
(defn find-peaks-valleys [{:keys [price] :as tick-list}]
  (reduce (fn [rslt ech]
            (let [fst (price (first ech))
                  snd (price (second ech))
                  thd (price (nth ech 2))
                  valley? (and (and (-> fst nil? not) (-> snd nil? not) (-> thd nil? not))
                               (> fst snd)
                               (< snd thd))
                  peak? (and (and (-> fst nil? not) (-> snd nil? not) (-> thd nil? not))
                             (< fst snd)
                             (> snd thd))]
              (cond
                peak? (conj rslt (assoc (second ech) :signal :peak))
                valley? (conj rslt (assoc (second ech) :signal :valley)))
              :else rslt)))
  []
  (partition 3 1 tick-list))

(defn calculate-strategy-a [rslt ech-list most-narrow upM? peaks valleys]
  (let [latest-diff (- (:upper-band (last ech-list)) (:lower-band (last ech-list)))
        less-than-any-narrow? (some (fn [inp] (< latest-diff (:difference inp))) most-narrow)]
    (if less-than-any-narrow?
      (if upM?
        (if (and (< (:last-trade-price (last ech-list)) (:upper-band (last ech-list)))
                 (> (:last-trade-price (last peaks))
                    (:upper-band (last (some #(= (:last-trade-time %)
                                                 (:last-trade-time (last peaks)))
                                             ech-list)))))
          (conj rslt (assoc (last ech-list) :signals [{:signal :down
                                                       :why :bollinger-close-abouve
                                                       :arguments [ech-list peaks]}]))
          (conj rslt (last ech-list)))
        (if (and (> (:last-trade-price (last ech-list)) (:lower-band (last ech-list)))
                 (< (:last-trade-price (last valleys))
                    (:lower-band (last (some #(= (:last-trade-time %)
                                                 (:last-trade-time (last valleys)))
                                             ech-list)))))
          (conj rslt (assoc (last ech-list) :signals [{:signal :up
                                                       :why :bollinger-close-below
                                                       :arguments [ech-list valleys]}]))
          (conj rslt (last ech-list)))))))

(defn calculate-strategy-b [rslt ech-list most-wide peaks valleys]
  (let [latest-diff (- (:upper-band (last ech-list)) (:lower-band (last ech-list)))
        more-than-any-wide? (some (fn [inp] (> latest-diff (:difference inp))) most-wide)]
    (if more-than-any-wide?
      ;; B iii RSI Divergence
      (let [OVER_BOUGHT 80
            OVER_SOLD 20
            rsi-list (base/relative-strength-index 14 ech-list)
            ;; i. price makes a higher high and
            higher-highPRICE? (if (empty? peaks)
                                false
                                (> (:last-trade-price (last ech-list))
                                   (:last-trade-price (last peaks))))
            ;; ii. rsi divergence makes a lower high
            lower-highRSI? (if (or (empty? peaks)
                                   (some #(nil? (:last-trade-time %)) rsi-list)
                                   #_(not (nil? rsi-list)))

                             false
                             (< (:rsi (last rsi-list))
                                (:rsi (last (filter (fn [inp]
                                                      (= (:last-trade-time inp)
                                                         (:last-trade-time (last peaks))))
                                                    rsi-list)))))
            ;; iii. and divergence should happen abouve the overbought line
            divergence-overbought? (> (:rsi (last rsi-list))
                                      OVER_BOUGHT)
            ;; i. price makes a lower low
            lower-lowPRICE? (if (or (empty? valleys)
                                    (some #(nil? (:last-trade-time %)) rsi-list))
                              false
                              (< (:last-trade-price (last ech-list))
                                 (:last-trade-price (last valleys))))
            higher-highRSI? (if (or (empty? valleys)
                                    (not (nil? rsi-list)))
                              false
                              (> (:rsi (last rsi-list))
                                 (:rsi (last (filter (fn [inp]
                                                       (= (:last-trade-time inp)
                                                          (:last-trade-time (last valleys))))
                                                     rsi-list)))))
            divergence-oversold? (< (:rsi (last rsi-list))
                                    OVER_SOLD)]
        (if (and higher-highPRICE? lower-highRSI? divergence-overbought?)
          (conj rslt (assoc (last ech-list) :signals [{:signal :down
                                                       :why :bollinger-divergence-overbought
                                                       :arguments [peaks ech-list rsi-list]}]))
          (if (and lower-lowPRICE? higher-highRSI? higher-highRSI? divergence-oversold?)
            (conj rslt (assoc (last ech-list) :signals [{:signal :up
                                                         :why :bollinger-divergence-oversold
                                                         :arguments
                                                         [valleys ech-list rsi-list]}]))
            (conj rslt (last ech-list)))))
      (conj rslt (last ech-list)))))


(defn bollinger-band-signals
  ([tick-list]
   (let [sma (base/simple-moving-average tick-list)
         bband (bollinger-band tick-list sma)]
     (bollinger-band-signals tick-list bband)))
  ([tick-list bband]
   (let [rslt []
         bbands ()
         sorted-bands (sort-bollinger-band bband)
         most-narrow (take 3 sorted-bands)
         most-wide (take-last 3 sorted-bands)
         partitioned-list (partition 2 1 (remove nil? tick-list)) ;; TODO check nil?
         upM? (trade-utils/up-market? 10 (remove nil? partitioned-list))
         downM? (trade-utils/down-market? 10 (remove nil? partitioned-list))
         side-market? (and (not upM?)
                           (not downM?))
         peaks-valleys (find-peaks-valleys (remove nil? tick-list))
         peaks (:peak (group-by :signal peaks-valleys))
         valleys (:valley (group-by :signal peaks-valleys))]
     (if (empty? (remove nil? tick-list))
       (conj rslt nil)
       (if (or upM? downM?)
         ;; A.
         (calculate-strategy-a rslt tick-list most-narrow upM? peaks valleys)
         ;; B.
         (calculate-strategy-b rslt tick-list most-wide peaks valleys)))
     (conj rslt (first bband)))))
