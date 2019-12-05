(ns adventofcode2019.day05
  (:require [clojure.string :as str]))

(defn parse
  [s]
  (->> (mapv #(Long/parseLong %) (str/split (str/trim s) #","))))

(defn mode-param
  "Interpret param according to mode."
  [program param mode]
  (case mode
    \0 (get program param)
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
  "Run one operation, returning the new [ip program input output] where output
  is optional."
  [program ip input]
  (let [p (subvec program ip)
        ins (str (first p))
        op-string (subs (str "0" ins) (dec (count ins)))
        [op min-len] (ops op-string)
        len (max min-len (- (count ins) 2))
        modes (concat (drop 2 (reverse ins)) (repeat \0))
        raw-params (take len (rest p))
        mode-params (map (partial mode-param program) raw-params modes)]
    '(binding [*out* *err*]
       (prn {:ip ip
             :ins ins
             :op op
             :raw-params raw-params
             :mode-params mode-params}))
    (case op
      :add           [(+ ip len 1)
                      (assoc program
                        (last raw-params) (apply + (drop-last mode-params)))
                      input]
      :multiply      [(+ ip len 1)
                      (assoc program
                        (last raw-params) (apply * (drop-last mode-params)))
                      input]
      :input         [(+ ip len 1)
                      (assoc program (first raw-params) (first input))
                      (rest input)]
      :output        [(+ ip len 1) program input (first mode-params)]
      :jump-if-true  [(if (zero? (first mode-params))
                        (+ ip len 1)
                        (second mode-params))
                      program
                      input]
      :jump-if-false [(if (zero? (first mode-params))
                        (second mode-params)
                        (+ ip len 1))
                      program
                      input]
      :less-than     [(+ ip len 1)
                      (assoc program
                        (last raw-params) (if (apply < (drop-last mode-params))
                                            1
                                            0))
                      input]
      :equals        [(+ ip len 1)
                      (assoc program
                        (last raw-params) (if (apply = (drop-last mode-params))
                                            1
                                            0))]
      :halt          [nil program input])))

(defn execute
  "Execute an entire program, printing output along the way."
  [program input]
  (loop [ip 0
         program (parse program)
         input input]
    (let [[ip program input output] (operate program ip input)]
      (when output (prn output))
      (if (nil? ip) {:value (first program)} (recur ip program input)))))

(def program (slurp "resources/day05.txt"))

(defn part-one
  []
  (execute program [1]))

(defn part-two
  []
  (execute program [5]))
