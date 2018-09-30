(ns t-bot.trade
  (:require
    [t-bot.utils.utils :as utils]

    [clojure.edn :as edn]
    [clojure.tools.logging :as log]
    ))


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
(defn fix-postion! [price qnt fee]
  { :id (utils/trade-counter)
    :time (new java.util.Date)
    :price price
    :qnt qnt
    :value (calculate-value price qnt fee)})

(defn open!
  [price qnt config]
  (let [ fee (:trading-fee config)
         open (fix-postion! price qnt fee)]
    (log/info (str "OPEN AMOUNT: " (:value open) "\nPRICE: " (:price open) "\nAT: " (:time open)))
    open))

(defn close!
  [price qnt config] ;; TODO: review price necessity...if we open with a market price
  (let [ fee (:trading-fee config)
         close (fix-postion! price qnt fee)]
    (log/info (str "CLOSE AMOUNT: " (:value close) "\nPRICE: " (:price close) "\nAT: " (:time close)))
    close))

(defn- parse-trade [trade]
  (let [keywordized (zipmap (map keyword (keys trade)) (vals trade))]
    (as-> keywordized t
      (update t :price (comp bigdec edn/read-string))
      (update t :qty edn/read-string)
      (assoc t :time (utils/parse-date (:time keywordized))))))

(defn- parse-response [response]
  (map parse-trade (:body response)))

(defn total-qnt [trade-list]
  (apply + (map :qnt trade-list)))

(defn total-value [trade-list]
  (apply + (map :value trade-list)))

;; TODO: unify
(defn get-ticks!
  ([platform symb] (get-ticks! platform symb 1))
  ([platform symb limit]
    (let [ endpoints (platform (utils/edn-slurp "resources/endpoints.edn"))
           url (utils/create-url (:ticks endpoints) (:base endpoints))]
      (parse-response (utils/get-response url {:symbol symb :limit limit})))))

(defn- try-to-open! [current-price qnt indicators config]
  (when (open? current-price indicators)
    (open! current-price qnt config)))

(defn- try-to-close! [open-list current-price indicators config]
  (when-let [ close-list (seq (filter
                                #(close? current-price (:price %) indicators)
                                open-list))]
    { :trade (close! current-price (total-qnt close-list) config)
      :population close-list}))
;; TODO: log history to file

(defn update-positions! [opens qnt indicators config]
  (let [ new-open (try-to-open! (:current-price indicators) qnt indicators config)
         new-close (try-to-close! (:population opens) (:current-price indicators) indicators config)
         cleared-list (utils/conj-some
                        (remove (set (:population new-close)) (:population opens))
                        new-open)]
    (-> opens
      (utils/assoc-some :last-open new-open)
      (utils/assoc-some :last-close (:trade new-close))
      (assoc :population cleared-list)
      (assoc :total-qnt (total-qnt cleared-list))
      (assoc :total-value (total-value cleared-list)))))
