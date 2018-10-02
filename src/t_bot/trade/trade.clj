(ns t-bot.trade.trade
  (:require
    [t-bot.auxiliary.utils :as utils]
    [t-bot.trade.utils :as trade-utils]

    [clojure.edn :as edn]
    [clojure.tools.logging :as log]

    [t-bot.trade.trade :as trade]))

;; TODO: UNIFY?
(defn open?
  ([current-price indicators]
    (open? current-price 0 indicators))
  ([current-price limit-ease {:keys [prev-price price-average lower-band signal] :as indicators}]
    (when-not (identical? :down signal)
      (let [ abs-limit-ease (with-precision 10 (* lower-band (/ limit-ease 100)))
             minimal-band (+ lower-band abs-limit-ease)]
        (<= current-price minimal-band)))))

(defn close?
  ([current-price indicators]
    (close? current-price 0 indicators))
  ([current-price open-price limit-ease {:keys [prev-price price-average upper-band signal] :as indicators}]
    (when-not (identical? :up signal)
      (let [ abs-limit-ease (with-precision 10 (* upper-band (/ limit-ease 100)))
             minimal-band (- upper-band abs-limit-ease)]
        (and (> current-price open-price) (>= current-price minimal-band))))))

;; TODO: remove after implementing api orders
(defn fix-postion! [price qnt fee]
  { :id (utils/trade-counter)
    :time (new java.util.Date)
    :price price
    :qnt qnt
    :value (trade-utils/calculate-value price qnt fee)})

(defn open!
  [price qnt trade-params config]
  (let [ open (fix-postion! price qnt (:fee trade-params))]
    (log/info (str "\n===>>> OPEN AMOUNT: " (:value open) " | PRICE: " (:price open) " | QNT: " qnt " | AT: " (:time open)) " ===>>>")
    open))

(defn close!
  [price qnt platform config] ;; TODO: review price necessity...if we open with a market price
  (let [ fee (get-in config [platform :trading-fee])
         close (fix-postion! price qnt fee)]
    (log/info (str "\n<<<=== CLOSE AMOUNT: " (:value close) " | PRICE: " (:price close) " | QNT: " qnt " | AT: " (:time close)) " <<<===")
    close))

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

(defn- try-to-open! [current-price trade-params config]
  (when (open? current-price (:limit-ease trade-params) (:indicators trade-params))
    (open! current-price (:qnt trade-params) platform config)))

(defn- try-to-close! [open-list current-price trade-params config]
  (when-let [ close-list (seq (filter
                                #(close? current-price (:price %) (:limit-ease trade-params) (:indicators trade-params))
                                open-list))]
    { :trade (close! current-price (trade-utils/get-total :qnt close-list) (:fee trade-params) config)
      :population close-list}))

;; TODO: log history to file

(defn update-positions! [opens trade-params config]
  (let [ new-open (try-to-open! (:current-price indicators) trade-params config)
         new-close (try-to-close! (:population opens) (:current-price indicators) indicators trade-params config)
         cleared-list (utils/conj-some
                        (remove (set (:population new-close)) (:population opens))
                        new-open)]
    (-> opens
      (utils/assoc-some :last-open new-open)
      (utils/assoc-some :last-close (:trade new-close))
      (assoc :population cleared-list)
      (assoc :total-qnt (trade-utils/get-total :qnt cleared-list))
      (assoc :total-value (trade-utils/get-total :value cleared-list)))))
