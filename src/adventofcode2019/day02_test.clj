(ns adventofcode2019.day02-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [adventofcode2019.day02 :refer :all]))

(def sample-program (parse "1,9,10,3,2,3,11,0,99,30,40,50"))

(deftest day02-one
  (is (= (operate 0 sample-program)
         [4 (parse "1,9,10,70,2,3,11,0,99,30,40,50")]))
  (is (= (execute sample-program) 3500)))
