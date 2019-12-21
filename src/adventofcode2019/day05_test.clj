(ns adventofcode2019.day05-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [adventofcode2019.intcode :refer :all]))

(defn diagnostic-code
  [& args]
  (-> (apply ->core args) run :output last))

(def sample-program "1,9,10,3,2,3,11,0,99,30,40,50")

(deftest day05-one (is (= (-> (->core sample-program) run :mem (get 0)) 3500)))

(def larger-example
  "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99")

(deftest day05-two
  ;; position mode
  (is (= (diagnostic-code "3,9,8,9,10,9,4,9,99,-1,8" [8]) 1))
  (is (= (diagnostic-code "3,9,8,9,10,9,4,9,99,-1,8" [7]) 0))
  (is (= (diagnostic-code "3,9,7,9,10,9,4,9,99,-1,8" [8]) 0))
  (is (= (diagnostic-code "3,9,7,9,10,9,4,9,99,-1,8" [7]) 1))
  ;; immediate mode
  (is (= (diagnostic-code "3,3,1108,-1,8,3,4,3,99" [8]) 1))
  (is (= (diagnostic-code "3,3,1108,-1,8,3,4,3,99" [7]) 0))
  (is (= (diagnostic-code "3,3,1107,-1,8,3,4,3,99" [8]) 0))
  (is (= (diagnostic-code "3,3,1107,-1,8,3,4,3,99" [7]) 1))
  ;; larger example
  (is (= (diagnostic-code larger-example [7]) 999))
  (is (= (diagnostic-code larger-example [8]) 1000))
  (is (= (diagnostic-code larger-example [9]) 1001)))
