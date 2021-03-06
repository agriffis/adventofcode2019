(defproject adventofcode2019 "0.1.0-SNAPSHOT"
  :description "Aron's solutions for Advent of Code 2019"
  :url "http://adventofcode.com/"
  :license {:name "Public Domain" :url "http://unlicense.org"}
  :dependencies [[net.mikera/core.matrix "0.62.0"]
                 [net.mikera/vectorz-clj "0.48.0"]
                 [org.clojure/clojure "1.10.1"]
                 [org.clojure/data.int-map "0.2.4"]
                 [org.clojure/math.combinatorics "0.1.6"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [primitive-math "0.1.6"]
                 [ubergraph "0.8.2"]]
  :main ^:skip-aot adventofcode2019.core
  :test-paths ["src" "test"])
