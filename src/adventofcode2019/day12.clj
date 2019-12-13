(ns adventofcode2019.day12
  (:require [adventofcode2019.contrib :refer [prime-factors-of]]))

;;; ----------------------------------------------------------------------
;;; Part 1
;;; ----------------------------------------------------------------------

(defn ->moons
  [s]
  (->> (re-seq #"[-\d]+" s)
       (mapv #(Long/parseLong %))
       (partition 3)
       (mapv #(vector % [0 0 0]))))

(def moons (->moons (slurp "resources/day12.txt")))

(defn gravity
  [m m2]
  (mapv compare (first m2) (first m)))

(defn combined-gravity
  [ms m]
  (->> (mapv (partial gravity m) ms)
       (apply map vector)
       (map (partial apply +))))

(defn step
  [ms]
  (let [gs (mapv (partial combined-gravity ms) ms)
        vs (mapv (partial map +) (map second ms) gs)
        ps (mapv (partial map +) (map first ms) vs)]
    (mapv vector ps vs)))

(defn potential-energy
  [m]
  (->> (first m) (mapv #(Math/abs %)) (apply +)))

(defn kinetic-energy
  [m]
  (->> (second m) (mapv #(Math/abs %)) (apply +)))

(defn energy
  [m]
  (* (potential-energy m) (kinetic-energy m)))

(defn total-energy
  [ms]
  (apply + (mapv energy ms)))

(defn part-one
  []
  (total-energy (-> (iterate step moons) (nth 1000))))

;;; ----------------------------------------------------------------------
;;; Part 2
;;; ----------------------------------------------------------------------

(defn repeats
  "Lazy sequence of indexes where the initial state repeats, starting with
  zero."
  [ms]
  (for [[hare i] (map vector (iterate step ms) (range))
        :when (= hare ms)]
    i))

(defn per-axis
  "Rotate moons from
    [[(x0 y0 z0) (dx0 dy0 dz0)] ; moon0
     [(x1 y1 z1) (dx1 dy1 dz1)] ; moon1
     [(x2 y2 z2) (dx2 dy2 dz2)] ; moon2
    ]
  to
   [[[[x0] [dx0]] [[x1] [dx1]] [[x2] [dx2]]] ; moons, x-axes only
    [[[y0] [dy0]] [[y1] [dy1]] [[y2] [dy2]]] ; moons, y-axes only
    [[[z0] [dz0]] [[z1] [dz1]] [[z2] [dz2]]] ; moons, z-axes only
   ]
  so we can find per-axis repeats."
  [ms]
  (->> (map (partial map (partial map vector)) ms)
       (map (partial apply map vector))
       (apply mapv vector)))

(defn lcm
  "Lowest common multiple by prime factorization.
  https://www.calculatorsoup.com/calculators/math/lcm.php"
  [& xs]
  (->> xs
       ;; Find all the prime factors of each given number.
       (map prime-factors-of)
       ;; List all the prime numbers found, as many times as they occur most
       ;; often for any one given number.
       (mapcat (partial group-by identity))
       (sort-by (comp count second))
       (into {}) ; discards lower counts
       (mapcat second)
       ;; Multiply the list of prime factors together to find the LCM.
       (apply *)))

(defn part-two
  []
  (let [periods (map (comp second repeats) (per-axis moons))]
    (apply lcm periods)))
