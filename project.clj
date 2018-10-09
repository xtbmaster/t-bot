(defproject t-bot "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
             :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [ [org.clojure/clojure "1.8.0"]
                  [clj-http "3.9.1"]
                  [throttler "1.0.0"]
                  [hswick/jutsu "0.1.2"]
                  [org.clojure/core.async "0.4.474"]

                  [org.clojure/math.numeric-tower "0.0.4"]
                  [org.apache.commons/commons-math3 "3.6.1"] ;;can be removed due to not using market-generator

                  [morse "0.4.0"]]
  :resource-paths ["resources"])
