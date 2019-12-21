(ns adventofcode2019.day07
  (:require [clojure.math.combinatorics :refer [permutations]]
            [clojure.string :as str]
            [adventofcode2019.intcode :refer :all]))

(def program (slurp "resources/day07.txt"))

(defn pipe
  [input cores]
  (rest (reductions (fn [prev core]
                      (run (update core :input concat (:output prev))))
                    {:output input}
                    cores)))

(defn amplify
  [core phases]
  (->> (map #(assoc core :input [%]) phases) (pipe [0]) last :output last))

(defn part1
  []
  (let [core (->core program)]
    (->> (for [phases (permutations [0 1 2 3 4])]
           (amplify core phases))
         (apply max))))

(defn feedback
  [core phases]
  (->> (loop [cores (map #(assoc core :input [%]) phases)
              input [0]]
         (let [cores (pipe input cores)]
           (if (nil? (:ip (last cores)))
             cores
             (recur cores (:output (last cores))))))
       last
       :output
       last))

(defn part2
  []
  (let [core (->core program)]
    (->> (for [phases (permutations [5 6 7 8 9])]
           (feedback core phases))
         (apply max))))
