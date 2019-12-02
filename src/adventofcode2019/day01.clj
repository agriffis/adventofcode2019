(ns adventofcode2019.day01
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]))

(def input (slurp "resources/day01.txt"))

(def modules
  (->> (str/split-lines input)
       (map #(Long/parseLong %))))

(defn module-fuel [m] (max 0 (- (quot m 3) 2)))

(defn modules-fuel
  [modules]
  (->> modules
       (map module-fuel)
       (apply +)))

(defn fuel-fuel
  [fuel]
  (->> (iterate module-fuel fuel)
       (take-while pos?)
       (apply +)))

(defn modules-fuel-with-fuel
  [modules]
  (->> modules
       (map module-fuel)
       (map fuel-fuel)
       (apply +)))

(deftest day01
  (is (= (module-fuel 12) 2))
  (is (= (module-fuel 100756) 33583))
  (is (= (modules-fuel modules) 3210097))
  (is (= (fuel-fuel (module-fuel 14)) 2))
  (is (= (fuel-fuel (module-fuel 1969)) 966))
  (is (= (fuel-fuel (module-fuel 100756)) 50346))
  (is (= (modules-fuel-with-fuel modules) 4812287)))
