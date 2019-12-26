(ns adventofcode2019.day18-test
  (:require [clojure.test :refer [deftest is]]
            [adventofcode2019.day18 :refer :all]))

(deftest day18
  (is (= 86 (shortest example1)))
  (is (= 132 (shortest example2)))
  (is (= 136 (shortest example3)))
  (is (= 32 (shortest example4))))
