(ns t-bot.trade
  (:require
    [clojure.edn :as edn]
    [t-bot.utils.utils :as utils]))


;; TODO: UNIFY?
(defn open?
  [current-price {:keys [prev-price upper-band lower-band signal] :as indicators}]
  (when-not (identical? :down signal)
    (<= current-price lower-band)))

(defn close?
  [current-price open-price {:keys [prev-price upper-band lower-band signal] :as indicators}]
  (when-not (identical? :up signal)
    (and (> current-price open-price) (>= current-price upper-band))))

(defn calculate-value
  ([price qnt] (calculate-value price qnt 0))
  ([price qnt fee-percentage]
    (let [ revenue (with-precision 10 (* price qnt))
           fee-amount (with-precision 10 (/ (* fee-percentage revenue) 100))]
      (- revenue fee-amount))))

;; TODO: remove after implementing api orders
(defn fix-postion!
  [price qnt fee]
  (let [id (utils/gen-id)]
    {id { :time (new java.util.Date)
          :price price
          :qnt qnt
          :value (calculate-value price qnt fee)}}))

(defn open!
  [price qnt config]
  (let [ fee (:trading-fee config)]
    (fix-postion! price qnt fee)))

(defn close!
  [price qnt config] ;; TODO: review price necessity...if we open with a market price
  (let [ fee (:trading-fee config)]
    (fix-postion! price qnt fee)))

(defn- parse-trade [trade]
  (let [keywordized (zipmap (map keyword (keys trade)) (vals trade))]
    (as-> keywordized t
      (update t :price (comp bigdec edn/read-string))
      (update t :qty edn/read-string)
      (assoc t :time (utils/parse-date (:time keywordized))))))

(defn- parse-response [response]
  (map parse-trade (:body response)))

;; TODO: unify
(defn get-ticks!
  ([platform symb] (get-ticks! platform symb 1))
  ([platform symb limit]
    (let [ endpoints (platform (utils/edn-slurp "resources/endpoints.edn"))
           url (utils/create-url (:ticks endpoints) (:base endpoints))]
      (parse-response (utils/get-response url {:symbol symb :limit limit})))))

(defn try-to-buy! [opens current-price qnt indicators config]
  (merge opens
    (when (open? current-price indicators)
      (open! current-price qnt config))))

(defn try-to-sell! [opens current-price qnt indicators config]
  (filter )
  (map (fn [current-price qnt indicators config]
         )
    opens)
  (dissoc opened-position
    (trade/close? current-price (:price opened-position) indicators)
    (trade/close! current-price qnt config))
