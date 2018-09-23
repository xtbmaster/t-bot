(ns t-bot.trade)

;; TODO; freefall
;; TODO: UNIFY
(defn open?
  [current-price {:keys [prev-price upper-band lower-band signal] :as indicators}]
  #_(when-not (identical? :down signal))
  (<= current-price lower-band))

(defn close?
  [current-price {:keys [prev-price upper-band lower-band signal open-price] :as indicators}]
  #_(when-not (identical? :up signal))
  (and (> current-price open-price (>= current-price upper-band))))

(defn fix-postion!
  [price qnt fee time position]
  { :position position
    :time time
    :price price
    :qnt qnt
    :value (calculate-value price qnt fee)})

(defn open!
  [price qnt fee time]
  (fix-postion! price qnt fee time :open))

(defn close!
  [price qnt fee time]
  (fix-postion! price qnt fee time :close))

(defn calculate-value
  ([price qnt] (expected-revenue price qnt 0))
  ([price qnt fee-percentage]
    (let [ revenue (with-precision 10 (* price qnt))
           fee-amount (with-precision 10 (/ (* fee-percentage revenue) 100))]
      (- revenue fee-amount))))
