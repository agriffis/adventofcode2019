(ns adventofcode2019.day21
  (:require [clojure.string :as str]
            [adventofcode2019.intcode :refer [->core run-ascii]]))

(def core (->core (slurp "resources/day21.txt")))

(defn parse
  [script]
  (-> script (str/replace #"-.*" "") (str/replace #"\s+\n" "\n") str/triml))

(defn part1
  []
  (let [input (parse (slurp "resources/day21a.txt"))]
    (-> (run-ascii core input) :output println)))

(defn part2
  []
  (let [input (parse (slurp "resources/day21b.txt"))]
    (-> (run-ascii core input) :output println)))
