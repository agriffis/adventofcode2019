(ns adventofcode2019.day05
  (:require [adventofcode2019.intcode :refer [->mem ->core run]]))

(def program (slurp "resources/day05.txt"))

(defn part-one
  []
  (-> (->mem program) (->core [1]) run :output))

(defn part-two
  []
  (-> (->mem program) (->core [5]) run :output))
