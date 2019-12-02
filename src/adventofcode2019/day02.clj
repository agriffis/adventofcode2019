(ns adventofcode2019.day02
  (:require [clojure.string :as str]))

(def input (slurp "resources/day02.txt"))

(defn parse-program
  [input]
  (->> (str/split (str/trim input) #",")
       (mapv #(Long/parseLong %))))

(def program (parse-program input))

(defn run-op
  ([ip program]
   (let [p (nthrest program ip)]
     (case (first p)
       1 (let [[i j t] (rest p)
               [a b] (map program [i j])]
           [(+ 4 ip) (assoc program t (+ a b))])
       2 (let [[i j t] (rest p)
               [a b] (map program [i j])]
           [(+ 4 ip) (assoc program t (* a b))])
       99 [nil (program 0)])))
  ([program] (run-op 0 program)))

(defn run-program
  [initial-program]
  (loop [ip 0
         program initial-program]
    (let [[ip result] (run-op ip program)]
      (if (nil? ip) result (recur ip result)))))

(defn try-program
  [noun verb]
  (run-program (-> program
                   (assoc 1 noun)
                   (assoc 2 verb))))

(defn first-half [] (try-program 12 2))

(defn second-half
  []
  (first (for [noun (range 100)
               verb (range 100)
               :when (= 19690720 (try-program noun verb))]
           (+ (* 100 noun) verb))))
