(ns adventofcode2019.day22
  (:require [clojure.string :as str]))

(def input (slurp "resources/day22.txt"))

(defn stack
  "When dealing a new stack, where does card i end up?"
  [c i]
  (- c i 1))

(defn cut
  "When cutting n cards, where does card i end up?"
  [c n i]
  (-> i (- n) (+ c) (mod c)))

(defn deal
  "When dealing with iteration n, where does card i end up?"
  [c n i]
  (-> (-> i (* n) (mod c))))

(defn shuffler
  "Make a shuffling function given input (shuffle program) and a number of
  cards. Calling the shuffling function with a card number returns the next
  location for that card."
  [input c]
  (->> (str/split-lines input)
       (map (fn [s]
              (condp re-matches s
                #"^deal with increment (\d+)$" :>>
                  (fn [[_ n]] (partial deal c (Long/parseLong n)))
                #"^cut (-?\d+)$" :>> (fn [[_ n]]
                                       (partial cut c (Long/parseLong n)))
                #"^deal into new stack$" (partial stack c))))
       reverse
       (apply comp)))

(defn part1
  "Where does card number 2019 go with one shuffle?"
  []
  ((shuffler input 10007) 2019))

(defn part2
  []
  ;; find the period for position 2020
  (count (take-while (partial not= 2020)
                     (rest (iterate (shuffler input 119315717514047) 2020)))))
