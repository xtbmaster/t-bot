(ns t-bot.utils.visualization
  (:require
    [jutsu.core :as j]))

(defn update-graph! [{:keys [name price time]}]
  (j/update-graph!
    name
    { :data {:y [[price]] :x [[time]]}
      :traces [0]}))

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
