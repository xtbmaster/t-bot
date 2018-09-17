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
