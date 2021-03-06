(ns t-bot.auxiliary.utils
  (:require
    [clj-time.coerce :as time-c]
    [clj-time.format :as time-f]

    [clj-http.client :as client]
    [clojure.string :as str]
    [clojure.edn :as edn]
    [throttler.core :refer [throttle-fn]]))

(def ^:private rate-limit 20)
(def ^:private throttle (throttle-fn identity rate-limit :second))

(defn create-url [query base-endpoint]
  (str base-endpoint query))

(defn safe-println
  "For concurrent console print."
  [& more]
  (.write *out* (str (str/join " " more) "\n")))

(defn logger [url]
  (safe-println "Calling: " url)
  url)

(defn get-response
  ([url] (get-response url {} true))
  ([url params] (get-response url params true))
  ([url params ex?]
    (-> url throttle logger
      (client/get { :as                 :auto
                    :query-params       params
                    :validate-redirects false
                    :cookie-policy      :standard
                    :throw-exceptions   ex?}))))

(defn parse-date [in-long]
  (let [ full-f (time-f/formatters :date-hour-minute-second-ms)
         short-f (time-f/formatters :hour-minute-second-ms)
         date-time (time-c/from-long in-long)
         full-date (time-f/unparse full-f date-time)
         short-date (time-f/unparse short-f date-time)]
    {:short short-date :full full-date}))

(defn make-time-unique [time id]
  (str (:short time) (rem id 100)))

(defn edn-slurp [url]
  (edn/read-string (clojure.core/slurp url)))

(defn- new-counter []
  (partial apply swap! (atom 0) inc []))

(def trade-counter (new-counter)) ;; FIXME: serialized trades between sessions

(defn conj-some
  "Like conj, but ignores xs that are nil."
  ([] [])
  ([coll] coll)
  ([coll & xs]
    (apply conj coll (remove nil? xs))))

(defn assoc-some
  "Like assoc, but skip the assoc if v is nil"
  [m k v]
  (if-not (nil? v)
    (assoc m k v)
    m))

(defn get-sum
  "Sums `k`eys of a `coll`ection of maps"
  [k coll]
  (->> coll
    (map k)
    (filter (complement nil?))
    (reduce +)))

(defn trim-vector [coll size-limit]
  (let [coll-size (count coll)
        surpass (- coll-size size-limit)]
    (if (and (> size-limit 0) (>= surpass 0))
      (vec (drop surpass coll))
      coll)))
