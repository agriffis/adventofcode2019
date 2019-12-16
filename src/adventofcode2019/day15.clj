(ns adventofcode2019.day15
  (:require [adventofcode2019.intcode :refer [->mem ->core run]]
            [ubergraph.core :as uber]
            [ubergraph.alg :as alg]))

(def tiles (vec "█.O "))

(defn draw
  [{:keys [grid droid] :or {droid []}}]
  (let [xs (mapv (comp first first) grid)
        left (apply min 0 xs)
        right (apply max 0 xs)
        ys (mapv (comp second first) grid)
        top (apply min 0 ys)
        bottom (apply max 0 ys)]
    (apply str
      (for [y (range top (inc bottom))
            x (range left (inc right))
            :let [tile (cond (= droid [x y]) \D
                             (and (zero? x) (zero? y)) \X
                             :else (tiles (get grid [x y] 3)))]]
        (str (when (and (= left x) (not= top y)) "\n") tile)))))

(def program (slurp "resources/day15.txt"))

(defn wall?
  [content]
  (assert content)
  (zero? content))

(defn passable?
  [content]
  (assert content)
  (not (wall? content)))

(defn oxygen?
  [content]
  (assert content)
  (= 2 content))

(defn coords-of
  "Find the coordinates [x y] in grid where pred? is satisfied."
  [pred? grid]
  (->> (filter (comp pred? second) grid) first first))

(def dirs [[0 -1] [0 1] [-1 0] [1 0]])

(defn adjust
  "Adjust coordinates by direction. Returns the new coordinates."
  [coords dir]
  (mapv + coords (dirs dir)))

(defn move
  "Try to move in the given direction."
  [{:keys [core droid grid] :as here} dir]
  (let [{:keys [output] :as core} (run (assoc core :input [(inc dir)]))
        content (first output)]
    (assert (= 1 (count output)))
    (assoc here
      :core core
      :droid (cond-> droid (passable? content) (adjust dir))
      :grid (assoc grid (adjust droid dir) content))))

(defn look
  "Look four directions without moving droid. Returns the updated grid."
  [{:keys [core grid droid] :as here}]
  (->> (for [dir (range 4)
             :when (nil? (grid (adjust droid dir)))]
         (:grid (move here dir)))
       (apply merge grid)))

(defn passages
  "Which directions can we go from here, excluding known spaces."
  [{:keys [core grid droid] :as here}]
  (let [ahead (look here)]
    (for [dir (range 4)
          :let [coords (adjust droid dir)]
          :when (and (nil? (grid coords)) (passable? (ahead coords)))]
      dir)))

(defn merge-grids
  "During exploration, we explore all the options independently then merge the
  grids. This returns a sequence with the grids merged forward to the end, so
  the last one is canonical."
  [heres]
  (reductions (fn [there here]
                (update here :grid (partial merge (:grid there))))
              heres))

(defn explore
  "Fully explore the dungeon."
  ([here] (explore here {}))
  ([here
    {:keys [custom-move custom-merge]
     :or {custom-move move custom-merge identity}
     :as options}]
   (loop [{:keys [core droid] :as here} here]
     (let [forks (passages here)
           here (assoc here :grid (look here))
           n (count forks)]
       (cond (zero? n) here
             (= n 1) (recur (custom-move here (first forks)))
             :else (->> (map (partial custom-move here) forks)
                        (map #(explore % options))
                        merge-grids
                        custom-merge
                        last))))))

(defn custom-move
  "When moving, also add an edge to the adjacency graph."
  [here dir]
  (let [src (:droid here)
        here (move here dir)
        dest (:droid here)]
    (cond-> here
      (not= src dest) (update :graph #(uber/add-edges % [src dest])))))

(defn custom-merge
  "When merging after exploring forks, also merge graphs forward, so that the
  last one is canonical."
  [heres]
  (reductions (fn [there here]
                (update here :graph (partial uber/build-graph (:graph there))))
              heres))

(defn ship
  []
  (let [here (explore {:core (-> program ->mem ->core)
                       :graph (uber/graph)
                       :grid {[0 0] 1}
                       :droid [0 0]}
                      {:custom-move custom-move :custom-merge custom-merge})]
    (println (draw here))
    here))

(defn part1
  []
  (let [{:keys [grid graph]} (ship)]
    (alg/shortest-path graph [0 0] (coords-of oxygen? grid))))

(defn part2
  []
  (let [{:keys [grid graph]} (ship)]
    (alg/longest-shortest-path graph (coords-of oxygen? grid))))
