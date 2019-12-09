(ns adventofcode2019.day05
  (:require [adventofcode2019.intcode :refer [->mem init run]]))

(def program (slurp "resources/day05.txt"))

(defn part-one
  []
  (-> (->mem program) (init [1]) run :output))

(defn part-two
  []
  (-> (->mem program) (init [5]) run :output))
