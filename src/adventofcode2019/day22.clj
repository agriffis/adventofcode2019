(ns adventofcode2019.day22
  (:require [clojure.math.numeric-tower :refer [expt]]
            [clojure.string :as str]))

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
  "When dealing with increment n, where does card i end up?"
  [c n i]
  (-> i (* n) (mod c)))

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

;;; https://github.com/metalim/metalim.adventofcode.2019.python/blob/master/22_cards_shuffle.ipynb

(defn parse-ops
  [input]
  (->>
    (str/split-lines input)
    (map (fn [s]
           (condp re-matches s
             #"^deal with increment (\d+)$" :>>
               (fn [[_ n]] (vector :deal (Long/parseLong n)))
             #"^cut (-?\d+)$" :>> (fn [[_ n]] (vector :cut (Long/parseLong n)))
             #"^deal into new stack$" (vector :stack))))))

(defn pow
  [b e m]
  (mod (expt b e) m))

(defn polypow
  [a b m n]
  (loop [a a
         b b
         m m
         n n]
    (cond (zero? m) [1 0]
          (even? m) (recur (* a (mod a n)) (mod (+ (* a b) b) n) (/ m 2) n)
          :else (let [[c d] (polypow a b (dec m) n)]
                  [(mod (* a c) n) (mod (+ (* a d) b) n)]))))

(defn part2
  "What does position 2020 contain after ridiculous shuffle?"
  []
  (let [pos 2020
        size 119315717514047N
        iterations 101741582076661
        [a b] (reduce (fn [[a b] [op n]]
                        (println a b)
                        (println op n)
                        (case op
                          :stack [(- a) (- size b 1)]
                          :cut [a (mod (+ b n) size)]
                          :deal (let [z (long (.modPow (biginteger n)
                                                       (biginteger (- size 2))
                                                       (biginteger size)))]
                                  [(mod (* a z) size) (mod (* b z) size)])))
                [1 0]
                (reverse (parse-ops input)))
        [a b] (polypow a b iterations size)]
    (mod (+ (* pos a) b) size)))
