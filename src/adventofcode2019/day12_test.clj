(ns adventofcode2019.day12-test
  (:require [clojure.test :refer [deftest is]]
            [adventofcode2019.day12 :refer :all]))

(def sample
  (->moons
    "<x=-1, y=0, z=2>\n<x=2, y=-10, z=-7>\n<x=4, y=-8, z=8>\n<x=3, y=5, z=-1>"))

(deftest day12-one
  (is (= (-> (iterate step sample) (nth 10))
         '([(2 1 -3) (-3 -2 1)]
           [(1 -8 0) (-1 1 3)]
           [(3 -6 1) (3 2 -3)]
           [(2 0 4) (1 -1 -1)])))
  (is (= (-> (iterate step sample) (nth 10) total-energy) 179)))

(deftest day12-two (is (= (second (repeats sample)) 2772)))
