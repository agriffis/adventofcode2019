(ns adventofcode2019.day14-test
  (:require [clojure.test :refer [deftest is]]
            [adventofcode2019.day14 :refer :all]))

(def sample1
  "10 ORE => 10 A
1 ORE => 1 B
7 A, 1 B => 1 C
7 A, 1 C => 1 D
7 A, 1 D => 1 E
7 A, 1 E => 1 FUEL")

(def g1 (-> sample1 ->rules ->graph))

(def sample2
  "9 ORE => 2 A
8 ORE => 3 B
7 ORE => 5 C
3 A, 4 B => 1 AB
5 B, 7 C => 1 BC
4 C, 1 A => 1 CA
2 AB, 3 BC, 4 CA => 1 FUEL")

(def g2 (-> sample2 ->rules ->graph))

(def sample3
  "157 ORE => 5 D
165 ORE => 6 H
44 A, 5 B, 1 C, 29 D, 9 E, 48 F => 1 FUEL
12 F, 1 E, 8 G => 9 C
179 ORE => 7 G
177 ORE => 5 F
7 H, 7 G => 2 A
165 ORE => 2 E
3 H, 7 D, 5 F, 10 G => 8 B")

(def g3 (-> sample3 ->rules ->graph))

(deftest day14-one
  (is (= 31 (ore g1)))
  (is (= 165 (ore g2)))
  (is (= 13312 (ore g3))))
