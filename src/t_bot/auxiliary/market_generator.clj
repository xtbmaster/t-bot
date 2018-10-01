(ns t-bot.auxiliary.market-generator
  (:require
    [clj-time.core :as tc]
    [clj-time.format :as tf])
  (:import [org.apache.commons.math3.distribution BetaDistribution]))

(def ^:private time-formatter (tf/formatters :time-no-ms))

(defn- random-in-range [lower upper]
  (let [r (rand upper)]
    (if (>= r lower)
      r
      (+ (rand (- upper lower)) lower))))

(defn- stochastic-k [last-price low-price high-price]
  (let [ hlrange (- high-price low-price)
         hlmidpoint (/ hlrange 2)
         numerator (if (> last-price hlmidpoint)
                     (- last-price hlmidpoint)
                     (- hlmidpoint low-price))]
    (if-not (zero? hlrange)
      (/ numerator hlrange)
      numerator)))

(defn- break-local-minima-maxima [k]
  (as-> k k
    (if (<= (double (+ 0.95 k)) 0)
      (+ 0.15 k) k)
    (if (>= k 1)
      (- k 0.15) k)))

(defn- generate-prices_
  ([low high]
   (generate-prices_ (random-in-range low high)))
  ([last-price]
   (iterate (fn [price]
              (let [ low (- price 5)
                     high (+ price 5)
                     k (stochastic-k price low high)
                     plus-OR-minus (rand-nth [- +])
                     kPM (if (= plus-OR-minus +)
                           (+ 1 (break-local-minima-maxima k))
                           (- 1 (break-local-minima-maxima k)))
                     newprice (* kPM price)
                     newlow (if (< newprice low) newprice low)
                     newhigh (if (> newprice high) newprice high)]
                newprice))
     last-price)))

(defn- generate-prices-essential [beginning-low beginning-high]
  (generate-prices_ beginning-low beginning-high))

(defn- polynomial [a b c x]
  (->
    (+ (* a
         (Math/pow x 3))
      (* b
        (Math/pow x 2)))
    (- (* c x))))

(defn- sine [a b d x]
  (- (* a
       (Math/sin (* b
                   (- x
                     (/ Math/PI 2)))))
    d))

(defn- polynomial-xintercept [x]
  (polynomial 2 2 3 x))

(defn- sine-xintercept [x]
  (sine 2 2 0 x))

(defn- ydirection [ypos]
  (if (pos? ypos)
    :positive
    :negative))

(defn- direction-changed? [ypos dirn]
  (not (= (ydirection ypos)
         dirn)))

(defn- get-opposite-direction-key [ydir]
  (if (= ydir :positive)
    :negative
    :positive))

(defn- get-opposite-direction-fn [dirn]
  (if (= dirn +) - +))

(defn- effectively-zero? [xval]
  (= 0.0 (Double. (format "%.7f" xval))))

(defn- find-xintercept [direction mfn]
  (loop [ start-point 0.0
          distance 1.0
          ydir (ydirection (mfn (direction 0 0.1)))
          dirn direction]
    (let [next-point (dirn start-point distance)]
      (if (effectively-zero? (mfn next-point))
        next-point
        (let [dc? (direction-changed? (mfn next-point) ydir)]
          (recur next-point
            (if dc? (/ distance 2) distance)
            (if dc? (get-opposite-direction-key ydir) ydir)
            (if dc? (get-opposite-direction-fn dirn) dirn)))))))

(defn- rand-double-in-range
  "Returns a random double between min and max."
  [min max]
  {:pre [(<= min max)]}
  (+ min (* (- max min) (Math/random))))

(defn- randomize-vertical-dilation [mathfn min' max']
  (let [a (rand-double-in-range min' max')]
    (partial mathfn a)))

(defn- randomize-horizontal-dilation [mathfn-curried min' max']
  (let [b (rand-double-in-range min' max')]
    (partial mathfn-curried b)))

(defn- generate-polynomial-sequence []
  (let [ one (randomize-vertical-dilation polynomial 0.5 2)
         two (randomize-horizontal-dilation one 0.5 2)
         polyn-partial (partial two 3)
         xinterc-polyn-left (find-xintercept - polynomial-xintercept)
         xinterc-polyn-right (find-xintercept + polynomial-xintercept)
         granularityP (rand-double-in-range 0.1 1)
         xsequenceP (iterate (partial + granularityP) xinterc-polyn-left)]
    (map polyn-partial xsequenceP)))

(defn- generate-sine-sequence []
  (let [ ein (randomize-vertical-dilation sine 0.5 2.7)
         zwei (randomize-horizontal-dilation ein 0.3 2.7)
         sine-partial (partial zwei 0)
         xinterc-sine-left (find-xintercept - sine-xintercept)
         xinterc-sine-right (find-xintercept + sine-xintercept)
         granularityS (rand-double-in-range 0.1 1)
         xsequenceS (iterate (partial + granularityS) xinterc-sine-left)]
    (map sine-partial xsequenceS)))

(defn- generate-oscillating-sequence []
  (generate-prices-essential 5 15))

(defn- sample-dispatcher [sample-type sample-length sample-fn]
  (take sample-length (sample-fn)))

(defn- sample-prices [beta-distribution]
  (let [sample-val (.sample beta-distribution)]
    (cond
      (< sample-val 0.33) (sample-dispatcher :sine (rand-double-in-range 10 15) generate-sine-sequence)
      (< sample-val 0.66) (sample-dispatcher :polynomial (rand-double-in-range 4 6) generate-polynomial-sequence)
      :else (sample-dispatcher :oscillating (rand-double-in-range 8 10) generate-oscillating-sequence))))

(defn- generate-prices-reductions [beta-distribution]
  (reductions (fn [ ^clojure.lang.LazySeq rslt
                    ^clojure.lang.LazySeq each-sample-seq]
                (let [ begining-price (if (empty? rslt)
                                       (rand-double-in-range 5 15)
                                       (last rslt))
                       sample-seq-head (first each-sample-seq)
                       price-difference (Math/abs (- sample-seq-head begining-price))]
                  (if (< sample-seq-head begining-price)
                    (concat rslt (map #(+ % price-difference) each-sample-seq))
                    (concat rslt (map #(- % price-difference) each-sample-seq) each-sample-seq))))
    '()
    (repeatedly #(sample-prices beta-distribution))))

(defn generate-prices
  ([] (generate-prices (BetaDistribution. 2.0 4.1)))
  ([beta-distribution]
   (map (fn [x] (if (neg? x) (* -1 x) x))
     (distinct (apply concat (generate-prices-reductions beta-distribution))))))

(defn- simplify-time [m]
  (let [ unparse (partial tf/unparse time-formatter)]
    (update m :time unparse)))

(defn generate-timeseries
  ([price-list]
   (generate-timeseries price-list (tc/now)))
  ([price-list datetime]
   (let [ unparse (partial tf/unparse time-formatter)
          time-list (map unparse (iterate #(tc/plus % (tc/seconds (rand 4))) datetime))
          splitted (apply map vector [time-list price-list])]
     (map #(zipmap [:time :price] %) splitted))))
