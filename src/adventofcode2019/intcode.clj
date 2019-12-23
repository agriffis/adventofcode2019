(ns adventofcode2019.intcode
  (:require [clojure.string :as str]
            [clojure.data.int-map :as i]))

(defn- ->mem
  "Parse comma-separated program string into memory map."
  [program]
  (->> (str/split (str/trim program) #",")
       (map #(Long/parseLong %))
       (map vector (range))
       (into (i/int-map))))

(defn- read-mem
  "Read param from memory map according to mode."
  [mem base param mode]
  (case mode
    \0 (get mem param 0)
    \1 param
    \2 (get mem (+ param base) 0)))

(defn- write-mem
  "Write to memory map according to mode."
  [mem base param mode value]
  (assoc mem
    (case mode
      \0 param
      \1 (throw (Exception. "write-mem mode 1"))
      \2 (+ param base))
      value))

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
(defn step
  "Run one operation, returning the new core."
  [{:keys [mem ip input output base] :as core}]
  (let [ins (str (mem ip))
        op-string (subs (str "0" ins) (dec (count ins)))
        [op min-len] (ops op-string)
        len (max min-len (- (count ins) 2))
        modes (take len (concat (drop 2 (reverse ins)) (repeat \0)))
        raw-params (mapv mem (range (inc ip) (+ len (inc ip))))
        params (mapv (partial read-mem mem base) raw-params modes)
        write (partial write-mem mem base (last raw-params) (last modes))]
    (merge core
           {:ip (+ ip len 1)}
           (case op
             :add           {:mem (write (apply + (drop-last params)))}
             :multiply      {:mem (write (apply * (drop-last params)))}
             :input         (if (seq input)
                              {:mem (write (first input)) :input (rest input)}
                              {:ip ip})
             :output        {:output (conj output (first params))}
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

(defn ->core
  "Boot a core, prepare to run."
  ([program] (->core program nil))
  ([program input] {:mem (->mem program) :input input :ip 0 :base 0}))

(defn run
  "Run core until finished or stuck."
  ([core input] (run (update core :input #(concat % input))))
  ([core]
   (loop [core (assoc core :output [])]
     (let [prev-ip (:ip core)
           {:keys [ip] :as core} (step core)]
       (if (or (nil? ip) (= ip prev-ip)) core (recur core))))))
