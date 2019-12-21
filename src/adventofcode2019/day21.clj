(ns adventofcode2019.day21
  (:require [clojure.string :as str]
            [adventofcode2019.intcode :refer [->core run]]))

(def core (->core (slurp "resources/day21.txt")))

(defn parse
  [script]
  (mapv int
    (-> script (str/replace #"-.*" "") (str/replace #"\s+\n" "\n") str/triml)))

(defn draw
  [output]
  (println (apply str (map char output))))

(defn part1
  []
  (let [input (parse (slurp "resources/day21a.txt"))]
    (-> core (assoc :input input) run :output draw)))

(defn part2
  []
  (let [input (parse (slurp "resources/day21b.txt"))]
    (-> core (assoc :input input) run :output draw)))
