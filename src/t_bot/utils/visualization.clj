(ns t-bot.utils.visualization
  (:require
    [jutsu.core :as j]))

(defn update-graph! [{:keys [ time average upper-band lower-band
                              price open-price close-price]} name]
  (j/update-graph!
    name
    { :data {:y [[price] [upper-band] [lower-band] [open-price] [close-price]] :x [[time] [time] [time] [time] [time]]} ;; check this
      :traces [0 1 2 3 4]}))

(defn build-graph!
  ([title] (build-graph! 3030 title))
  ([port title]
    (let [ ju (j/start-jutsu! port false)
           _ (println ju)]
      (j/graph!
        title
        [{ :x []
           :y []
           :type "scatter"}]))))

(j/graph!
  "TEST-DATA"
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
                :symbol "circle-open"}}])
