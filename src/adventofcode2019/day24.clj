(ns adventofcode2019.day24
  (:require [clojure.core.matrix :refer :all]
            [clojure.string :as str]))

(def input (slurp "resources/day24.txt"))

(def cells {\. 0 \# 1})

(defn parse
  [s]
  (->> s (filter cells) (map cells) (partition (.indexOf s "\n")) array))

(defn frame
  ([a] (frame a 0 0 0 0))
  ([a top right bottom left]
   (as-> a $
     (join-along 0 (broadcast top [1 5]) $ (broadcast bottom [1 5]))
     (join-along 1 (broadcast left [7 1]) $ (broadcast right [7 1])))))

(defn unframe
  [a]
  (array (submatrix a 1 (- (row-count a) 2) 1 (- (column-count a) 2))))

(defn fate
  [x & neighbors]
  (let [sum (int (apply + neighbors))]
    (if (zero? x) (if (< 0 sum 3) 1 0) (if (= 1 sum) 1 0))))

(defn step-
  [a]
  (emap fate
        a
        (shift a [1 0])
        (shift a [0 1])
        (shift a [-1 0])
        (shift a [0 -1])))

(defn step
  [a]
  (-> a frame step- unframe))

(defn powers-of
  [n]
  (iterate (partial * n) 1))

(def powers-matrix (->> (powers-of 2) (partition 5) (take 5) array))

(defn biodiversity
  [a]
  (long (esum (emul a powers-matrix))))

(defn draw
  [a]
  (pm a {:formatter (comp str [\. \#] int)}))

(defn part1
  ([] (part1 input))
  ([input]
   (let [a (parse input)
         as (iterate step a)
         bs (map biodiversity as)
         seens (reductions conj #{} bs)
         [dup bio] (first (->> (map vector as bs seens)
                               (drop-while (fn [[a b seen]] (not (seen b))))))]
     (draw dup)
     bio)))

(def dpad {:up [2 1] :right [3 2] :down [2 3] :left [1 2] :center [2 2]})

(defn dget
  [a d]
  (let [[x y] (dpad d)]
    (mget a y x)))

(defn dset
  [a d v]
  (let [[x y] (dpad d)]
    (mset a y x v)))

(def zeroes (zero-matrix 5 5))

(defn step2-
  [above a below]
  (-> (apply frame a (map (partial dget above) [:up :right :down :left]))
      step-
      unframe
      (dset :up
            (apply fate
              (dget a :up)
              (mget a 1 1)
              (mget a 0 2)
              (mget a 1 3)
              (get-row below 0)))
      (dset :right
            (apply fate
              (dget a :right)
              (mget a 1 3)
              (mget a 2 4)
              (mget a 3 3)
              (get-column below 4)))
      (dset :down
            (apply fate
              (dget a :down)
              (mget a 3 1)
              (mget a 4 2)
              (mget a 3 3)
              (get-row below 4)))
      (dset :left
            (apply fate
              (dget a :left)
              (mget a 1 1)
              (mget a 2 0)
              (mget a 3 1)
              (get-column below 0)))
      (dset :center 0)))

(defn step2
  [aa]
  (cond-> aa
    ;; If the highest layer has live bugs, stack another layer on top.
    (some pos? (eseq (first aa))) (as-> $ (concat [zeroes] $))
    ;; If the lowest layer has live bugs, stack another layer underneath.
    (some pos? (eseq (peek aa))) (as-> $ (concat $ [zeroes]))
    true (as-> $ (mapv step2-
                   ;; Each layer's result depends on itself, plus the layers
                   ;; above and below. The highest and lowest layers depend on
                   ;; a temporary layer of zeroes for their calculations.
                   (concat [zeroes] (drop-last $))
                   $
                   (concat (rest $) [zeroes])))))

(defn count-bugs
  [aa]
  (apply + (map (comp long esum) aa)))

(defn part2
  ([] (part2 input))
  ([input]
   (let [a (parse input)
         aas (iterate step2 [a])
         final (nth aas 200)]
     (count-bugs final))))
