(ns adventofcode2019.day05
  (:require [adventofcode2019.intcode :refer [->core run]]))

(def program (slurp "resources/day05.txt"))

(defn part-one
  []
  (-> (->core program [1]) run :output))

(defn part-two
  []
  (-> (->core program [5]) run :output))
