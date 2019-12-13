(ns adventofcode2019.day13
  (:require [adventofcode2019.intcode :refer [->mem init run]]
            [clojure.pprint :refer [pprint]]))

(def program (slurp "resources/day13.txt"))

(defn block?
  [[[x y] t]]
  (= 2 t))

(defn paddle?
  [[[x y] t]]
  (= 3 t))

(defn ball?
  [[[x y] t]]
  (= 4 t))

(defn x-of
  [pred? grid]
  (let [[[x y] t] (first (filter pred? grid))]
    x))

(defn play
  ([core] (play core {} 0))
  ([core grid score]
   (let [{:keys [ip output] :as core} (run core)
         [grid score]
           (reduce (fn [[g s] [x y t]]
                     (if (= [-1 0] [x y]) [g t] [(assoc g [x y] t) s]))
             [grid score]
             (partition 3 output))]
     (cons [grid score]
           (when ip
             (let [ball-x (x-of ball? grid)
                   paddle-x (x-of paddle? grid)]
               (lazy-seq (play (assoc core :input [(compare ball-x paddle-x)])
                               grid
                               score))))))))

(defn part-one
  []
  (->> (play (-> program ->mem init))
       last ; game state
       first ; grid
       (filter block?)
       count))

(defn part-two
  []
  (->> (play (-> program ->mem (assoc 0 2) init))
       last ; game state
       second ; score
    ))

;;; extra

(def tiles (vec " █▒━o"))

(defn draw
  [grid]
  (let [xs (mapv (comp first first) grid)
        left (apply min xs)
        right (apply max xs)
        ys (mapv (comp second first) grid)
        top (apply min ys)
        bottom (apply max ys)]
    (doseq [y (range top (inc bottom))
            x (range left (inc right))]
      (when (and (= left x) (not= top y)) (println))
      (print (tiles (get grid [x y] 0))))
    (println)))
