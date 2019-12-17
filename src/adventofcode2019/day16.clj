(ns adventofcode2019.day16
  (:require [clojure.math.numeric-tower :as math]
            [clojure.string :as str]))

(defn split
  [s]
  (mapv #(- (bigint (long %)) (bigint (long \0))) (str/trim s)))

(def input (split (slurp "resources/day16.txt")))

(def base [0 1 0 -1])

(defn patterns
  [len]
  (for [i (range len)]
    (->> (apply mapcat vector (repeat (inc i) base))
         cycle
         rest
         (take len)
         vec)))

(defn indexes-of
  [pred? coll]
  (->> (map-indexed vector coll) (filter (comp pred? second)) (mapv first)))

(defn calcs
  [patterns]
  (for [p patterns
        :let [pluses (indexes-of pos? p)
              minuses (indexes-of neg? p)]]
    (fn [input]
      (let [sum (- (apply + (map input pluses)) (apply + (map input minuses)))]
        (mod (math/abs sum) 10)))))

(defn phase
  [fs input]
  (mapv #(% input) fs))

(defn part1
  [input n]
  (let [fs (->> (patterns (count input)) calcs)]
    (apply str (take 8 (nth (iterate (partial phase fs) input) n)))))

(def input2 (vec (mapcat identity (repeat 10000 input))))

(defn phase2
  [ri]
  (reductions + ri))

(defn part2
  [input]
  (let [offset (Long/parseLong (apply str (take 7 input)))
        input (subvec input offset)
        final (reverse (nth (iterate phase2 (rseq input)) 100))
        message (apply str (map #(mod % 10) (take 8 final)))]
    message))
