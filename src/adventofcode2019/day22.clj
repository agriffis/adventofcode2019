(ns adventofcode2019.day22
  (:require [clojure.string :as str]))

(def input (slurp "resources/day22.txt"))

(defn stack
  [deck]
  (vec (rseq deck)))

(defn deal
  [n deck]
  (reduce (fn [d i] (assoc d (-> i (* n) (rem (count d))) (deck i)))
    deck
    (range (count deck))))

(defn cut
  [n deck]
  (let [i (cond-> n (neg? n) (+ (count deck)))]
    (vec (concat (subvec deck i) (subvec deck 0 i)))))

(defn parse
  [input]
  (->>
    (str/split-lines input)
    (map (fn [s]
           (condp re-matches s
             #"^deal with increment (\d+)$" :>>
               (fn [[_ n]] (partial deal (Long/parseLong n)))
             #"^cut (-?\d+)$" :>> (fn [[_ n]] (partial cut (Long/parseLong n)))
             #"^deal into new stack$" stack)))))

(defn part1
  []
  (-> (reduce #(%2 %1) (vec (range 10007)) (parse input)) (.indexOf 2019)))
