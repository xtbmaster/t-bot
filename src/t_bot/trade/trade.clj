(ns t-bot.trade.trade
  (:require
    [t-bot.auxiliary.utils :as utils]
    [t-bot.trade.utils :as trade-utils]

    [clojure.tools.logging :as log]))

;; TODO: UNIFY?
(defn open?
  ([current-price indicators]
    (open? current-price 0 indicators))
  ([current-price limit-factor {:keys [sma lower-band signal]}]
    (when-not (= :down (:prev signal) (:current signal))
      (let [adjusted-limit (trade-utils/adjusted-limit sma lower-band limit-factor)]
        (<= current-price adjusted-limit)))))

(defn close?
  ([current-price open-price indicators]
    (close? current-price open-price 0 indicators))
  ([current-price open-price limit-factor {:keys [prev-price sma upper-band signal]}]
    (when-not (= :up (:prev signal (:current signal)))
      (let [adjusted-limit (trade-utils/adjusted-limit sma upper-band limit-factor)]
        (and (> current-price open-price) (>= current-price adjusted-limit))))))

;; TODO: remove after implementing api orders
(defn fix-postion! [price qnt fee]
  { :id (utils/trade-counter)
    :time (new java.util.Date)
    :price price
    :qnt qnt
    :value (trade-utils/calculate-value price qnt fee)})

(defn open!
  ([price qnt config] (open! price qnt 0 config))
  ([price qnt fee config]
    (let [ {:keys [value price time] :as open} (fix-postion! price qnt fee)]
      (log/info (str "\n===>>> OPEN AMOUNT: " value " | PRICE: " price " | QNT: " qnt " | AT: " time) " ===>>>")
      open)))

(defn close!
  ([price qnt config] (close! price qnt 0 config)) ;; TODO: review price necessity...if we open with a market price
  ([price qnt fee config]
    (let [ {:keys [value price time] :as close} (fix-postion! price qnt fee)]
      (log/info (str "\n<<<=== CLOSE AMOUNT: " value " | PRICE: " price " | QNT: " qnt " | AT: " time) " <<<===")
      close)))

(defn- try-to-open!
  [{:keys [indicators limit-factor qnt fee signals], {:keys [current-price]} :indicators} config]
  (when (open? current-price limit-factor indicators)
    (open! current-price qnt fee config)))

(defn- try-to-close!
  [open-list {:keys [indicators limit-factor fee signals], {:keys [current-price]} :indicators} config]
  (when-let [ close-list (seq (filter
                                #(close? current-price (:price %) limit-factor indicators)
                                open-list))]
    { :trade (close! current-price (trade-utils/get-total :qnt close-list) fee config)
      :population close-list}))

;; TODO: log history to file

(defn update-positions! [{:keys [population] :as opens} {:keys [opens-limit] :as trade-params} config]
  (let [ new-open (when (< (count population) opens-limit)
                    (try-to-open! trade-params config))
         {:keys [close-trade] :as new-close} (try-to-close! population trade-params config)
         cleared-list (utils/conj-some
                        (remove (set (:population new-close)) population)
                        new-open)]
    (-> opens
      (utils/assoc-some :last-open new-open)
      (utils/assoc-some :last-close close-trade)
      (assoc :population cleared-list)
      (assoc :total-qnt (trade-utils/get-total :qnt cleared-list))
      (assoc :total-value (trade-utils/get-total :value cleared-list)))))
