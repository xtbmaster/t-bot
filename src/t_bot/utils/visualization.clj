(ns t-bot.utils.visualization
  (:require
    [jutsu.core :as j]))

(defn update-graph! [{:keys [ time price-average price-exponential upper-band lower-band
                              current-price open-price close-price signal] :as indicators} name]
  (let [ n-data (count indicators)
         y-axis [ [current-price] [upper-band] [lower-band]
                  [price-average] [price-exponential] [open-price] [close-price]
                  [signal]]
         x-axis (vec (repeat n-data [(:short time)]))
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

(defn start-jutsu! [port]
  (j/start-jutsu! port false))

(defn graph! [name]
  (j/graph!
    name
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
        :marker {:line { :width 3}
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
        :marker { :line { :width 3}
                  :size 10
                  :symbol "circle-close"}}]))
