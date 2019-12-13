(ns adventofcode2019.day11
  (:require [adventofcode2019.intcode :refer [->mem init run]]))

(def program (slurp "resources/day11.txt"))

(def turns [-1 1])

(def directions [[0 -1] [1 0] [0 1] [-1 0]])

(defn turtle
  [[[x y d] painted] [color turn]]
  (let [painted (assoc painted [x y] color)
        d (-> (+ d (turns turn) 4) (mod 4))
        [x y] (map + [x y] (directions d))]
    [[x y d] painted]))

(defn paint
  ([program] (paint program {}))
  ([program painted] (paint (init (->mem program) []) [0 0 0] painted))
  ([core robot painted]
   (lazy-seq
     (let [{:keys [ip output] :as core} (run core)
           [robot painted] (reduce turtle [robot painted] (partition 2 output))]
       (when ip
         (let [[x y] robot
               input (get painted [x y] 0)
               core (assoc core :input [input])]
           (cons [robot painted] (paint core robot painted))))))))

(def colors (vec " █+"))

(defn hull
  [painted]
  (let [minx (reduce min 0 (->> (keys painted) (map first)))
        maxx (reduce max 0 (->> (keys painted) (map first)))
        miny (reduce min 0 (->> (keys painted) (map second)))
        maxy (reduce max 0 (->> (keys painted) (map second)))]
    (run! (fn [y]
            (run! (fn [x] (print (colors (get painted [x y] 2))))
                  (range (dec minx) (+ 2 maxx)))
            (println))
          (range (dec miny) (+ 2 maxy)))))

(defn part-one
  []
  (-> (paint program) last second count))

(defn part-two
  []
  (-> (paint program {[0 0] 1}) last second hull))
