(ns adventofcode2019.day20
  (:require [adventofcode2019.contrib :refer [cond-let]]
            [clojure.string :as str]
            [ubergraph.core :as uber]
            [ubergraph.alg :as alg]))

(def floor? (partial = \.))

(def portal? (set "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"))

(defn cell?
  [c]
  (or (floor? c) (portal? c)))

(defn ->grid
  [s]
  (let [lines (str/split-lines s)]
    (->> (for [[line y] (map vector lines (range))
               [c x] (map vector line (range))
               :when (cell? c)]
           [[x y] c])
         (into {}))))

(defn proximal
  [grid pred? [x y]]
  (filter (comp pred? grid) [[(dec x) y] [(inc x) y] [x (dec y)] [x (inc y)]]))

(defn portal-label
  [grid [x y]]
  (assert portal? (grid [x y]))
  (let [cs (->> [[x y]]
                (concat (proximal grid portal? [x y]))
                ;; labels are read left-to-right and top-to-down
                (sort-by first)
                (sort-by second)
                (map grid))]
    (assert (= (count cs) 2))
    (apply str cs)))

(defn grid-nodes
  [grid]
  (for [[xy c] grid
        :when (floor? c)
        :let [pxy (first (proximal grid portal? xy))
              label (when pxy (portal-label grid pxy))]]
    [xy
     (cond-> {}
       label (assoc :portal (str/upper-case label))
       label (assoc :inner (not (Character/isUpperCase (first label)))))]))

(defn ->graph
  [grid]
  (let [nodes (grid-nodes grid)
        direct-edges (->> (for [xy (map first nodes)
                                :let [neighbors (proximal grid floor? xy)]]
                            (map (partial vector xy) neighbors))
                          (mapcat vec))
        portal-edges (->> (group-by (comp :portal second) nodes)
                          (filter first) ; drop nil
                          (map second) ; node pairs
                          (map (fn [x] (assert (< (count x) 3) x) x))
                          (filter second) ; drop AA ZZ
                          (map (partial mapv first)))]
    (-> (uber/graph)
        (uber/add-nodes-with-attrs* nodes)
        (uber/add-edges* direct-edges)
        (uber/add-edges* portal-edges))))

(defn find-node
  [g pred?]
  (let [nodes (->> (uber/nodes g) (filter pred?))]
    (assert (= (count nodes) 1))
    (first nodes)))

(defn find-portal
  [g label]
  (find-node g #(= (uber/attr g % :portal) label)))

(def example1 (slurp "resources/day20x1.txt"))

(def example2 (slurp "resources/day20x2.txt"))

(def input (slurp "resources/day20.txt"))

(defn part1
  [input]
  (let [g (-> input ->grid ->graph)]
    (-> (alg/shortest-path g (find-portal g "AA") (find-portal g "ZZ"))
        alg/cost-of-path)))

(defn z-nodes
  [g [x y z]]
  (assert z)
  (for [edge (uber/out-edges g [x y])
        :let [src (uber/src edge)
              dest (uber/dest edge)
              dest-z (cond (or (not (uber/attr g src :portal))
                               (not (uber/attr g dest :portal)))
                             z
                           (uber/attr g src :inner) (inc z)
                           :else (dec z))]
        :when (not (neg? dest-z))]
    {:dest (conj dest dest-z)}))

(defn part2
  [input]
  (let [g (-> input ->grid ->graph)]
    (-> (alg/shortest-path (partial z-nodes g)
                           (conj (find-portal g "AA") 0)
                           (conj (find-portal g "ZZ") 0))
        alg/cost-of-path)))
