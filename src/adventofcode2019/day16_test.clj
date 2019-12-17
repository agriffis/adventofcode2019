(ns adventofcode2019.day16-test
  (:require [clojure.test :refer [deftest is]]
            [adventofcode2019.day16 :refer :all]))

(def sample1 (split "12345678"))

(def sample2 (split "80871224585914546619083218645595"))

(def sample5
  (split (apply str (repeat 10000 "03036732577212944063491565474664"))))

(deftest day16
  (is (= "01029498" (part1 sample1 4)))
  (is (= "24176176" (part1 sample2 100)))
  (is (= "84462026" (part2 sample5))))
