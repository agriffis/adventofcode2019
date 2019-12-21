(ns adventofcode2019.day19
  (:require [adventofcode2019.intcode :refer [->core run]]))

(def core (->core (slurp "resources/day19.txt")))

(defn part1
  []
  (->> (for [x (range 50)
             y (range 50)]
         (-> core (assoc :input [x y]) run :output first))
       (remove zero?)
       count))

(defn in-beam?
  [x y]
  (-> core (assoc :input [x y]) run :output first (= 1)))

(def in-space? (complement in-beam?))

(defn part2
  []
  (loop [x 0
         y 99]
    (let [x (first (drop-while #(in-space? % y) (iterate inc x)))]
      (if (in-beam? (+ x 99) (- y 99)) [x (- y 99)] (recur x (inc y))))))
