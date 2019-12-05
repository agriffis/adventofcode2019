(ns adventofcode2019.day05
  (:require [clojure.string :as str]))

(defn parse
  [s]
  (->> (mapv #(Long/parseLong %) (str/split (str/trim s) #","))))

(defn mode-param
  "Interpret param according to mode."
  [mem param mode]
  (case mode
    \0 (get mem param)
    \1 param))

(def ops
  {"01" [:add 3]
   "02" [:multiply 3]
   "03" [:input 1]
   "04" [:output 1]
   "05" [:jump-if-true 2]
   "06" [:jump-if-false 2]
   "07" [:less-than 3]
   "08" [:equals 3]
   "99" [:halt 0]})

;!zprint {:format :next :pair {:justify? true} :vector {:wrap? false}}
(defn operate
  "Run one operation, returning the new [ip mem input output] where output
  is optional."
  [mem ip input]
  (let [p (subvec mem ip)
        ins (str (first p))
        op-string (subs (str "0" ins) (dec (count ins)))
        [op min-len] (ops op-string)
        len (max min-len (- (count ins) 2))
        modes (concat (drop 2 (reverse ins)) (repeat \0))
        rps (take len (rest p))
        mps (map (partial mode-param mem) rps modes)]
    '(binding [*out* *err*]
       (prn {:ip ip :ins ins :op op :rps rps :mps mps}))
    (case op
      :add           [(+ ip len 1)
                      (assoc mem (last rps) (apply + (drop-last mps)))
                      input]
      :multiply      [(+ ip len 1)
                      (assoc mem (last rps) (apply * (drop-last mps)))
                      input]
      :input         [(+ ip len 1)
                      (assoc mem (first rps) (first input))
                      (rest input)]
      :output        [(+ ip len 1) mem input (first mps)]
      :jump-if-true  [(if (zero? (first mps)) (+ ip len 1) (second mps))
                      mem
                      input]
      :jump-if-false [(if (zero? (first mps)) (second mps) (+ ip len 1))
                      mem
                      input]
      :less-than     [(+ ip len 1)
                      (assoc mem (last rps) (if (apply < (drop-last mps)) 1 0))
                      input]
      :equals        [(+ ip len 1)
                      (assoc mem (last rps) (if (apply = (drop-last mps)) 1 0))]
      :halt          [nil mem input])))

(defn execute
  "Execute an entire program, printing output along the way."
  [program input]
  (loop [ip 0
         mem (parse program)
         input input]
    (let [[ip mem input output] (operate mem ip input)]
      (when output (prn output))
      (if (nil? ip) {:value (first mem)} (recur ip mem input)))))

(def program (slurp "resources/day05.txt"))

(defn part-one
  []
  (execute program [1]))

(defn part-two
  []
  (execute program [5]))
