(ns adventofcode2019.day23
  (:require [adventofcode2019.contrib :refer [cond-let]]
            [adventofcode2019.intcode :refer [->core run]]))

(def core (->core (slurp "resources/day23.txt")))

(defn concatv
  "Non-lazy concat to vector, avoids stack overflows."
  [& xs]
  (reduce into (vec (first xs)) (rest xs)))

(defn route
  "Route packets to core inputs. Returns updated cores."
  [cores packets]
  (reduce (fn [cores [addr & xy]]
            (cond-> cores
              (< addr (count cores)) (update-in [addr :input] concatv xy)))
    cores
    packets))

(defn run-network
  "Run the network through one set of inputs, routing packets to core inputs for
  the next run."
  [cores nat]
  (let [cores (mapv #(run % [-1]) cores)
        packets (partition-all 3 (mapcat :output cores))]
    [(route cores packets) (concatv nat (filter #(= 255 (first %)) packets))]))

(defn part1
  "Run the network until something is routed to the nat."
  []
  (loop [cores (mapv #(assoc core :input [%]) (range 50))]
    (let [[cores nat] (run-network cores [])]
      (or (peek nat) (recur cores)))))

(defn part2
  "Run the network until the nat injects the same packet twice in a row."
  []
  (loop [cores (mapv #(assoc core :input [%]) (range 50))
         last-injected nil]
    (cond-let :let [[cores nat] (run-network cores [])]
              (some (comp seq :input) cores) (recur cores last-injected)
              :let [inject (peek nat)]
              (= inject last-injected) inject
              :else (recur (assoc-in cores [0 :input] (rest inject)) inject))))
