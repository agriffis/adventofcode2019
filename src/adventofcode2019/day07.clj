(ns adventofcode2019.day07
  (:require [clojure.math.combinatorics :refer [permutations]]
            [clojure.string :as str]
            [adventofcode2019.intcode :refer :all]))

(def program (slurp "resources/day07.txt"))

(defn amplify
  [mem phases input]
  (->> (map #(init mem [%]) phases) (pipe [0]) last :output last))

(defn part-one
  []
  (let [mem (->mem program)]
    (->> (for [phases (permutations [0 1 2 3 4])]
           (amplify mem phases [0]))
         (apply max))))

(defn screech
  [mem phases input]
  (->> (loop [cores (map #(init mem [%]) phases)
              input input]
         (let [cores (pipe input cores)]
           (if (nil? (:ip (last cores)))
             cores
             (recur cores (:output (last cores))))))
       last
       :output
       last))

(defn part-two
  []
  (let [mem (->mem program)]
    (->> (for [phases (permutations [5 6 7 8 9])]
           (screech mem phases [0]))
         (apply max))))
