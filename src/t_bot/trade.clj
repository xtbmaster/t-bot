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

(defn try-to-open! [opens current-price qnt indicators config]
  (if (open? current-price indicators)
    (let [ opened-postition (open! current-price qnt config)
           population (merge (:population opens) opened-position)]
      (-> opens
        (assoc :last-trade opened-position)
        (assoc :population population)
        (update :total-qnt + (:qnt opened-position))
        (update :total-value + (:value opened-position))))
    opens))

(defn try-to-close! [opens current-price indicators config]
  (when-let [ close-list (seq (filter #(close? current-price (:price %) indicators) opens))]
    { :last-trade (close! current-price (total-qnt close-list) config)
      :population close-list
      :total-qnt (total-qnt close-list)
      :total-value (total-value close-list)}))

;; (defn try-to-close! [opens current-price indicators config]
;;   (let [ grouped-list (as-> opens os
;;                         (group-by #(close? current-price (:price %) indicators) os)
;;                         (clojure.set/rename-keys os {true :closed false :opened}))
;;          qnt-to-close (total-qnt (:closed grouped-list))]
;;     (update grouped-list :closed (close! current-price qnt-to-close config))))


;; { :open-positions { :population [{ :id
;;                                    :time
;;                                    :price
;;                                    :qnt
;;                                    :value}]
;;                     :total-qnt :total-qnt-of-opened-postions
;;                     :total-value :total-value}}

;; {:closed-positions { :population [{ :id [:list-of-maps-of-OPENED-positions]
;;                                     :total-qnt
;;                                     :total-value}]
;;                      :total-qnt :total-qnt-of-closed-postions
;;                      :total-value :total-value}}
