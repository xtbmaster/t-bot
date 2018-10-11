(ns t-bot.auxiliary.visualization
  (:require
    [jutsu.core :as j]))

  (defn update-graph! [name portfolio indicators time]
    (let [data (merge indicators
                      unique-time
                      {:open-price (-> portfolio :last-open :price)}
                      {:close-price (-> portfolio :last-close :price)})
          filtered-data (cond-> data
                           (= (:last-open opens) (:last-open portfolio)) (dissoc data :open-price)
                           (= (:last-close opens) (:last-close portfolio)) (dissoc data :close-price))
          ;; FIXME signals
          {:keys [ time sma ema upper-band lower-band
                  current-price open-price close-price signal]} filtered-data
          n-data 8 ;; TODO: check if it is possible to count data
          cross (and (:current signal) ema) ;; FIXME

          y-axis [ [current-price] [upper-band] [lower-band]
                   [sma] [ema] [open-price] [close-price]
                   [cross]]
          x-axis (vec (repeat n-data [time]))
          traces (vec (range n-data))]

    (j/update-graph!
      name
      { :data { :y y-axis
                :x x-axis}
        :traces traces})))

;; (defn build-graph!
;;   ([title] (build-graph! 3030 title))
;;   ([port title]
;;     (let [ ju (j/start-jutsu! port false)
;;            _ (println ju)]
;;       (j/graph!
;;         title
;;         [{ :x []
;;            :y []
;;            :type "scatter"}]))))

(def graphic-structure

  [ { :x []
      :y []
      :type "line"
      :name "price"}

    { :x []
      :y []
      :type "line"
      :name "upper-band"
      :line { :dash "dashdot"}}

    { :x []
      :y []
      :type "line"
      :name "lower-band"
      :line { :dash "dashdot"}}

    { :x []
      :y []
      :type "line"
      :name "sma"}

    { :x []
      :y []
      :type "line"
      :name "ema"}

    { :x []
      :y []
      :type "scatter"
      :mode "markers"
      :name "open"
      :marker { :line { :width 3}
                :opacity 0.5
                :size 12
                :symbol "circle-open"}}

    { :x []
      :y []
      :type "scatter"
      :mode "markers"
      :name "close"
      :marker { :line { :width 3}
                :opacity 0.5
                :size 12
                :symbol "circle-close"}}

    { :x []
      :y []
      :type "scatter"
      :mode "markers"
      :name "cross"
      :marker { :line { :width 2}
                :size 8
                :opacity 0.8
                :symbol "circle-close"}}])


(defn start-jutsu! [port]
  (j/start-jutsu! port false))

(defn graph! [name]
  (j/graph!
    name ;id
    graphic-structure ;data
    nil ;layout
    ))
