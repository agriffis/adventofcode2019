(ns adventofcode2019.day09-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [adventofcode2019.intcode :refer [->core run]]))

(defn boot
  [program input]
  (-> (->core program input) run))

(def sample1 "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99")

(def sample2 "1102,34915192,34915192,7,4,7,99,0")

(def sample3 "104,1125899906842624,99")

(deftest day09-one
  (is (= sample1 (->> (:output (boot sample1 [])) (map str) (str/join ","))))
  (is (= 16 (->> (:output (boot sample2 [])) first str count)))
  (is (= 1125899906842624 (->> (:output (boot sample3 [])) first))))
