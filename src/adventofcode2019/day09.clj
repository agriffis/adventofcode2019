(ns adventofcode2019.day09
  (:require [adventofcode2019.intcode :refer [->mem init run]]))

(def program (slurp "resources/day09.txt"))

(defn part-one
  []
  (-> (->mem program) (init [1]) run :output))

(defn part-two
  []
  (-> (->mem program) (init [2]) run :output))
