(ns adventofcode2019.day08
  (:require [clojure.string :as str]))

(defn ->image
  [s]
  (mapv #(- (int %) (int \0)) (str/trim s)))

(def mine (->image (slurp "resources/day08.txt")))

(def layers (partition (* 25 6) mine))

(defn count-digit
  [d layer]
  (->> layer (filter #(= d %)) count))

(defn part-one
  []
  (let [layer (->> layers (sort-by (partial count-digit 0)) first)]
    (* (count-digit 1 layer) (count-digit 2 layer))))

(defn pixel
  [& xs]
  (first (drop-while #(= 2 %) xs)))

(defn render
  [layer w]
  (->> (replace {0 " " 1 "█"} layer)
       (partition w)
       (map #(apply str %))
       (str/join "\n")))

(defn part-two
  []
  (let [layered (apply (partial map pixel) layers)]
    (println (render layered 25))))
