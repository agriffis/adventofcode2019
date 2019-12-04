(ns adventofcode2019.day04
  (:require [clojure.string :as str]))

(def input "307237-739058")

(defn part-one
  [input]
  (let [[start end] (map #(Long/parseLong %) (str/split input #"-"))]
    (->> (range start (inc end))
         (map str)
         (filter #(re-matches #"0*1*2*3*4*5*6*7*8*9*" %))
         (filter #(re-matches #".*?(.)\1.*" %)))))

(count (part-one input))

(defn part-two
  [input]
  (->> (part-one input)
       (filter #(re-matches #"(.)\1(?!\1).*|.*?(.)(?!\2)(.)\3(?!\3).*" %))))

(count (part-two input))
