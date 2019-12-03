(ns adventofcode2019.day03
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]))

(defn turtle
  "Given an instruction (D10) and current point, return the next point."
  [s [x y]]
  (let [d (subs s 0 1)
        v (Long/parseLong (subs s 1))]
    (case d
      "D" [x (- y v)]
      "L" [(- x v) y]
      "R" [(+ x v) y]
      "U" [x (+ y v)])))

(defn wire
  "A wire is a sequence of points [x y] starting at [0 0]."
  [s]
  (->> (str/split s #",")
       (reduce (fn [path v] (conj path (turtle v (peek path)))) [[0 0]])))

(defn wires
  [input]
  (map wire (str/split-lines input)))

(def input (wires (slurp "resources/day03.txt")))

(defn segments
  "Convert a wire (points) to a sequence of segments [[x1 y1] [x2 y2]]."
  [wire]
  (map vector wire (rest wire)))

(defn vert?
  [segment]
  (apply = (map first segment)))

(defn horiz?
  [segment]
  (apply = (map second segment)))

(defn intersection
  "If there is an intersection between two segments, return the intersection
  point."
  [horiz vert]
  (cond (and (horiz? horiz) (vert? vert))
          (let [[x1 x2] (map first horiz)
                [x1 x2] (if (< x1 x2) [x1 x2] [x2 x1])
                [y1 y2] (map second vert)
                [y1 y2] (if (< y1 y2) [y1 y2] [y2 y1])
                [[_ hy]] horiz
                [[vx]] vert]
            (when (and (>= vx x1) (<= vx x2) (>= hy y1) (<= hy y2)) [vx hy]))
        (and (horiz? vert) (vert? horiz)) (intersection vert horiz)))

(defn manhattan
  [point]
  (->> point (map #(Math/abs %)) (apply +)))

(defn combinations
  "[a b c] => [[a b] [a c] [b c]]"
  [items]
  (for [subitems (drop-last (take-while seq (iterate rest items)))
        :let [i (first subitems)]
        j (rest subitems)]
    [i j]))

(defn intersections
  "Intersecting points between two wires."
  ([wires]
   (->> wires
        (map segments)
        combinations
        (mapcat (partial apply intersections))))
  ([segments1 segments2]
   (->> (for [s1 segments1
              s2 segments2]
          (intersection s1 s2))
        rest ; [0, 0]
        (remove nil?))))

(defn closest-intersection
  [wires]
  (first (sort-by manhattan (intersections wires))))

(defn first-half
  [input]
  (manhattan (closest-intersection input)))

(defn segment-steps
  "How many steps in a segment?"
  [[[x1 y1] [x2 y2]]]
  (+ (Math/abs (- x2 x1)) (Math/abs (- y2 y1))))

(defn steps
  "Map points to steps, taking shortcuts for points already seen."
  [segments]
  (first (reduce (fn [[seen prev] segment]
                   (let [p (second segment)
                         s (min (get seen p Long/MAX_VALUE)
                                (+ prev (segment-steps segment)))]
                     [(assoc seen p s) s]))
           [{} 0]
           segments)))

(defn intersection-steps
  "Combined steps for each intersecting point between two wires."
  ([wires]
   (->> wires
        (map segments)
        (map #(vector % (steps %)))
        combinations
        (mapcat (partial apply intersection-steps))))
  ([[segments1 steps1] [segments2 steps2]]
   (for [sub-segments1 (rest (reductions conj [] segments1))
         sub-segments2 (rest (reductions conj [] segments2))
         :when (or (seq (rest sub-segments1)) (seq (rest sub-segments2)))
         :let [s1 (peek sub-segments1)
               s2 (peek sub-segments2)
               i (intersection s1 s2)]
         :when i]
     (+ (steps1 (second (peek (pop sub-segments1))))
        (segment-steps [(first s1) i])
        (steps2 (second (peek (pop sub-segments2))))
        (segment-steps [(first s2) i])))))

(defn second-half
  [input]
  (first (sort (intersection-steps input))))

(def sample1 (wires "R8,U5,L5,D3\nU7,R6,D4,L4\n"))

(def sample2
  (wires (str "R75,D30,R83,U83,L12,D49,R71,U7,L72\n"
              "U62,R66,U55,R34,D71,R55,D58,R83\n")))

(def sample3
  (wires (str "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\n"
              "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7\n")))

(deftest day03
  (is (= sample1
         '([[0 0] [8 0] [8 5] [3 5] [3 2]] [[0 0] [0 7] [6 7] [6 3] [2 3]])))
  (is (= (segments (first sample1))
         '([[0 0] [8 0]] [[8 0] [8 5]] [[8 5] [3 5]] [[3 5] [3 2]])))
  (is (= (combinations [1 2 3]) [[1 2] [1 3] [2 3]]))
  (is (= (manhattan (closest-intersection sample1)) 6))
  (is (= (manhattan (closest-intersection sample2)) 159))
  (is (= (manhattan (closest-intersection sample3)) 135))
  (is (= (first-half input) 280))
  (is (= (intersection-steps sample1) '(30 40)))
  (is (= (second-half sample1) 30))
  (is (= (second-half sample2) 610))
  (is (= (second-half sample3) 410))
  (is (= (second-half input) 10554)))
