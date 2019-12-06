(ns adventofcode2019.day06-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [adventofcode2019.day06 :refer :all]))

(def sample-one (slurp "resources/day06-sample-one.txt"))

(def sample-two (slurp "resources/day06-sample-two.txt"))

(deftest day06
  (is (= (count-orbits (parse sample-one)) 42))
  (is (= (count-transfers (parse sample-two) "YOU" "SAN") 4)))
