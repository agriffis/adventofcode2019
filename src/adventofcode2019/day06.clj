(ns adventofcode2019.day06
  (:require [clojure.string :as str]))

(defn parse
  [s]
  (->> (str/split-lines s)
       (map #(str/split % #"\)"))
       (map reverse)
       (map vec)
       (into {})))

(def not-nil? (complement nil?))

(defn orbits
  [m o]
  (take-while not-nil? (iterate m o)))

(defn count-orbits
  [m]
  (->> (keys m) (map #(orbits m %)) (map rest) (map count) (apply +)))

(def input (slurp "resources/day06.txt"))

(defn part-one
  []
  (count-orbits (parse input)))

(defn count-transfers
  [m a b]
  (let [orbs (mapcat #(rest (orbits m %)) [a b])
        gross (count orbs)
        net (count (distinct orbs))
        common (- gross net)]
    (- net common)))

(defn part-two
  []
  (count-transfers (parse input) "YOU" "SAN"))
