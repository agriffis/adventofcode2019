(ns adventofcode2019.day01
  (:require [clojure.string :as str]))

(def input (slurp "resources/day01.txt"))

(def module-masses
  (->> (str/split-lines input)
       (map #(Long/parseLong %))))

(defn fuel-required [m] (max 0 (- (quot m 3) 2)))

(def initial-fuel-requirement
  (->> module-masses
       (map fuel-required)
       (apply +)))

(defn fuel-required-with-fuel
  [fuel]
  (->> (iterate fuel-required fuel)
       (take-while pos?)
       (apply +)))

(def extra-fuel-requirement
  (->> module-masses
       (map fuel-required)
       (map fuel-required-with-fuel)
       (apply +)))

[initial-fuel-requirement extra-fuel-requirement]
