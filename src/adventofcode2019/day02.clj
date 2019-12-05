(ns adventofcode2019.day02
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]))

(defn parse
  [input]
  (->> (str/split (str/trim input) #",") (mapv #(Long/parseLong %))))

(defn operate
  [ip program]
  (let [p (nthrest program ip)]
    (case (first p)
      1 (let [[i j t] (rest p)
              [a b] (map program [i j])]
          [(+ 4 ip) (assoc program t (+ a b))])
      2 (let [[i j t] (rest p)
              [a b] (map program [i j])]
          [(+ 4 ip) (assoc program t (* a b))])
      99 [nil (program 0)])))

(defn execute
  [program]
  (loop [ip 0
         program program]
    (let [[new-ip result] (operate ip program)]
      (if (nil? new-ip) result (recur new-ip result)))))

(defn try-program
  [program noun verb]
  (execute (-> program (assoc 1 noun) (assoc 2 verb))))

(def program (parse (slurp "resources/day02.txt")))

(defn part-one
  []
  (try-program program 12 2))

(defn part-two
  []
  (first (for [noun (range 100)
               verb (range 100)
               :when (= 19690720 (try-program program noun verb))]
           (+ (* 100 noun) verb))))
