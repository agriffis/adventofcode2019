(ns adventofcode2019.day07
  (:require [clojure.math.combinatorics :refer [permutations]]
            [clojure.string :as str]))

(defn ->mem
  "Parse comma-separated program into memory vector."
  [program]
  (->> (mapv #(Long/parseLong %) (str/split (str/trim program) #","))))

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
       (prn {:ip ip :ins ins :op op :rps rps :mps mps :input input :mem mem}))
    (case op
      :add           [(+ ip len 1)
                      (assoc mem (last rps) (apply + (drop-last mps)))
                      input]
      :multiply      [(+ ip len 1)
                      (assoc mem (last rps) (apply * (drop-last mps)))
                      input]
      :input         (if (seq input)
                       [(+ ip len 1)
                        (assoc mem (first rps) (first input))
                        (rest input)]
                       [ip mem input])
      :output        [(+ ip len 1) mem input [(first mps)]]
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

(defn amplify
  "Run amplifier until finished or output or stuck."
  [{:keys [ip mem input]}]
  (loop [ip ip
         mem mem
         input input]
    (let [[next-ip mem input output] (operate mem ip input)]
      (if (or (nil? next-ip) output (= next-ip ip))
        {:ip next-ip :mem mem :input input :output output}
        (recur next-ip mem input)))))

(defn pipeline
  "Run amplifiers in a pipeline."
  [amps input]
  (rest (reductions (fn [prev amp]
                      (amplify (update amp :input concat (:output prev))))
                    {:output input}
                    amps)))

(defn amplifier
  "Initialize amplifier with phase."
  [mem phase]
  {:ip 0 :mem mem :input [phase]})

(def mine (->mem (slurp "resources/day07.txt")))

(defn part-one
  []
  (->> (for [phases (permutations [0 1 2 3 4])
             :let [amps (map (partial amplifier mine) phases)]]
         (pipeline amps [0]))
       (map (comp last :output last))
       (apply max)))

(defn feedback
  "Run amplifiers in a feedback loop."
  [amps input]
  (loop [amps amps
         input input
         last-output nil]
    (let [{:keys [ip output]} (last amps)
          last-output (or output last-output)]
      (if (nil? ip)
        {:amps amps :output (last last-output)}
        (let [amps (pipeline amps input)
              output (-> amps last :output)]
          (recur amps output last-output))))))

(defn part-two
  []
  (->> (for [phases (permutations [5 6 7 8 9])
             :let [amps (map (partial amplifier mine) phases)]]
         (feedback amps [0]))
       (map :output)
       (apply max)))
