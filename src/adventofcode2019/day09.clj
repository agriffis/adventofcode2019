(ns adventofcode2019.day09
  (:require [adventofcode2019.intcode :refer [->core run]]))

(def program (slurp "resources/day09.txt"))

(defn part-one
  []
  (-> (->core program [1]) run :output))

(defn part-two
  []
  (-> (->core program [2]) run :output))
