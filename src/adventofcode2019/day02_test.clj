(ns adventofcode2019.day02-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [adventofcode2019.day02 :refer :all]))

(def sample-program (parse-program "1,9,10,3,2,3,11,0,99,30,40,50"))

(defn is-unparsed [[ip p] b] (let [a [ip (str/join "," p)]] (is a b)))

(deftest day02
  (is-unparsed (run-op sample-program) [4 "1,9,10,70,2,3,11,0,99,30,40,50"])
  (is (run-program sample-program) 30)
  (is (first-half) 5305097)
  (is (second-half) 4925))
