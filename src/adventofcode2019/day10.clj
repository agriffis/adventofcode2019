(ns adventofcode2019.day10
  (:require [clojure.pprint :refer [pprint]]
            [clojure.set :as set]
            [clojure.string :as str]))

;;; ----------------------------------------------------------------------
;;; Part 1
;;; ----------------------------------------------------------------------

(defn ->points
  [s]
  (for [[y row] (map-indexed vector (str/split-lines s))
        [x chr] (map-indexed vector row)
        :when (= chr \#)]
    [x y]))

(defn quadrant?
  [[x y]]
  (and (> x 0) (>= y 0)))

(defn clockwise
  [[x y]]
  (vector (- y) x))

(defn rat
  [[x y]]
  (if (zero? y) Long/MAX_VALUE (/ x y)))

(defn visible-quadrant
  [ps]
  (->> (filter quadrant? ps) (map rat) distinct count))

(defn visible-points
  [ps]
  (->> (take 4 (iterate (partial map clockwise) ps))
       (map visible-quadrant)
       (apply +)))

(defn origin
  [[ox oy] [px py]]
  (vector (- px ox) (- py oy)))

(defn visible-from-point
  [ps p]
  (visible-points (map (partial origin p) ps)))

(defn best-location
  [ps]
  (->> (map #(vector (visible-from-point ps %) %) ps)
       (sort-by (comp - first))
       first))

(def points (->points (slurp "resources/day10.txt")))

(defn part-one
  []
  (best-location points))

;;; ----------------------------------------------------------------------
;;; Part 2
;;; ----------------------------------------------------------------------

(defn counterclockwise
  [[x y]]
  (vector y (- x)))

(defn unorigin
  [o p]
  (origin (map - o) p))

(defn shoot-quadrant
  [ps p q]
  (->> ps
       (map (partial origin p))
       (map clockwise)
       (map (apply comp (repeat q counterclockwise)))
       (filter quadrant?)
       (sort-by (partial apply +))
       (group-by rat)
       (sort-by (comp - first))
       (map (comp first second))
       (map (apply comp (repeat q clockwise)))
       (map counterclockwise)
       (map (partial unorigin p))))

(defn asteroids
  [ps p]
  (lazy-seq (if (empty? ps)
              nil
              (let [vaporized (mapcat (partial shoot-quadrant ps p) (range 4))
                    remaining (set/difference (set ps) (set vaporized))]
                (concat vaporized (asteroids remaining p))))))

(defn part-two
  []
  (nth (asteroids points [11 19]) 199))
