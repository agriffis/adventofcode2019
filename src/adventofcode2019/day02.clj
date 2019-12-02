(ns adventofcode2019.day02
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]))

(defn parse-program
  [input]
  (->> (str/split (str/trim input) #",")
       (mapv #(Long/parseLong %))))

(def program (parse-program (slurp "resources/day02.txt")))

(defn run-op
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

(defn run-program
  [program]
  (loop [ip 0
         program program]
    (let [[new-ip result] (run-op ip program)]
      (if (nil? new-ip) result (recur new-ip result)))))

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

(def sample-program (parse-program "1,9,10,3,2,3,11,0,99,30,40,50"))

(deftest day02
  (is (run-op 0 sample-program)
      [4 (parse-program "1,9,10,70,2,3,11,0,99,30,40,50")])
  (is (run-program sample-program) 30)
  (is (first-half) 5305097)
  (is (second-half) 4925))
