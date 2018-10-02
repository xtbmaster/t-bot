(ns t-bot.trade.utils)

(defn calculate-value
  ([price qnt] (calculate-value price qnt 0))
  ([price qnt fee-percentage]
   (let [ revenue (with-precision 10 (* price qnt))
          fee-amount (with-precision 10 (/ (* fee-percentage revenue) 100))]
     (- revenue fee-amount))))

(defn get-total [indicator trade-list]
  (apply + (map indicator trade-list)))
