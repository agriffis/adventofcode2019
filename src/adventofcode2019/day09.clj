(ns adventofcode2019.day09
  (:require [clojure.string :as str]))

(defn ->mem
  "Parse comma-separated program string into memory vector."
  [s]
  (->> (str/split (str/trim s) #",")
       (map #(Long/parseLong %))
       (into (vector-of :long))))

(defn read-mem
  "Read param from memory vector according to mode."
  [mem base param mode]
  (case mode
    \0 (get mem param 0)
    \1 param
    \2 (get mem (+ param base) 0)))

(defn write-mem
  "Write to memory vector according to mode, growing as needed to avoid out of
  bounds."
  [mem base param mode value]
  (let [index (case mode
                \0 param
                \1 nil
                \2 (+ param base))
        mem (apply conj mem (repeat (max 0 (- (inc index) (count mem))) 0))]
    (assoc mem index value)))

(def ops
  {"01" [:add 3]
   "02" [:multiply 3]
   "03" [:input 1]
   "04" [:output 1]
   "05" [:jump-if-true 2]
   "06" [:jump-if-false 2]
   "07" [:less-than 3]
   "08" [:equals 3]
   "09" [:adjust-base 1]
   "99" [:halt 0]})

;!zprint {:format :next :pair {:justify? true} :vector {:wrap? false}}
(defn operate
  "Run one operation, returning the new state."
  [{:keys [mem ip input output base] :as state}]
  (let [p (subvec mem ip)
        ins (str (first p))
        op-string (subs (str "0" ins) (dec (count ins)))
        [op min-len] (ops op-string)
        len (max min-len (- (count ins) 2))
        modes (take len (concat (drop 2 (reverse ins)) (repeat \0)))
        raw-params (take len (rest p))
        params (map (partial read-mem mem base) raw-params modes)
        write (partial write-mem mem base (last raw-params) (last modes))]
    (merge state
           {:ip (+ ip len 1)}
           (case op
             :add           {:mem (write (apply + (drop-last params)))}
             :multiply      {:mem (write (apply * (drop-last params)))}
             :input         (if (seq input)
                              {:mem (write (first input)) :input (rest input)}
                              {:ip ip})
             :output        {:output (concat output [(first params)])}
             :jump-if-true  {:ip (if (zero? (first params))
                                   (+ ip len 1)
                                   (second params))}
             :jump-if-false {:ip (if (zero? (first params))
                                   (second params)
                                   (+ ip len 1))}
             :less-than     {:mem (write (if (apply < (drop-last params)) 1 0))}
             :equals        {:mem (write (if (apply = (drop-last params)) 1 0))}
             :adjust-base   {:base (+ base (first params))}
             :halt          {:ip nil}))))

(defn execute
  "Run program until finished or stuck."
  [state]
  (loop [state state]
    (let [prev-ip (:ip state)
          {:keys [ip] :as state} (operate state)]
      (if (or (nil? ip) (= ip prev-ip)) state (recur state)))))

(defn initialize
  "Initialize Intcode computer state."
  ([mem] (initialize mem nil))
  ([mem input] {:ip 0 :base 0 :input input :mem mem}))

(def program (slurp "resources/day09.txt"))

(defn part-one
  []
  (:output (execute (initialize (->mem program) [1]))))

(defn part-two
  []
  (:output (execute (initialize (->mem program) [2]))))
