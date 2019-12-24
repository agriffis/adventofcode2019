(ns adventofcode2019.day24
  (:require [clojure.core.matrix :refer :all]
            [clojure.string :as str]))

(set-current-implementation :vectorz)

(def input (slurp "resources/day24.txt"))

(def cells {\. 0 \# 1})

(defn parse
  [s]
  (->> s (filter cells) (map cells) (partition (.indexOf s "\n")) array))

(defn mget*
  ([a pos] (mget* a pos 0))
  ([a pos default]
   (try
     (apply mget a pos)
     (catch IndexOutOfBoundsException e default))))

(defn pad
  [a]
  (as-> (zero-array (map (partial + 2) (shape a))) a'
    (emap-indexed (partial mget* a) a')
    (shift a' [-1 -1])))

(defn unpad
  [a]
  (submatrix a 1 (- (row-count a) 2) 1 (- (column-count a) 2)))

(defn live-or-die
  [a]
  (emap (fn [x & others]
          (if (zero? x)
            (if (< 0 (apply + others) 3) 1 0)
            (if (== 1 (apply + others)) 1 0)))
        a
        (shift a [1 0])
        (shift a [0 1])
        (shift a [-1 0])
        (shift a [0 -1])))

(defn step
  [a]
  (-> (live-or-die a)
      (set-row 0 0)
      (set-row (dec (row-count a)) 0)
      (set-column 0 0)
      (set-column (dec (column-count a)) 0)))

(defn powers-of
  [n]
  (iterate (partial * n) 1))

(defn biodiversity
  [a]
  (->> (unpad a) (mapcat (partial map int)) (map * (powers-of 2)) (apply + 0)))

(defn draw
  [a]
  (pm a {:formatter (comp str [\. \#] int)}))

(defn part1
  []
  (let [a (pad (parse input))
        as (iterate step a)
        seens (reductions conj #{} as)
        dup (first (->> (map vector as seens)
                        (drop-while (fn [[a seen]] (not (seen a))))
                        (map first)))]
    (draw dup)
    (biodiversity dup)))
