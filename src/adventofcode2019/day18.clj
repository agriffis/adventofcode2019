(ns adventofcode2019.day18
  (:require [adventofcode2019.contrib :refer [cond-let]]
            [adventofcode2019.graph :refer [trim-non-path-nodes join-hallways]]
            [clojure.string :as str]
            [ubergraph.core :as uber]
            [ubergraph.alg :as alg]))

;!zprint {:format :skip}
(def example1 "
########################
#f.D.E.e.C.b.A.@.a.B.c.#
######################.#
#d.....................#
########################
")

;!zprint {:format :skip}
(def example2 "
########################
#...............b.C.D.f#
#.######################
#.....@.a.B.c.d.A.e.F.g#
########################
")

;!zprint {:format :skip}
(def example3 "
#################
#i.G..c...e..H.p#
########.########
#j.A..b...f..D.o#
########@########
#k.E..a...g..B.n#
########.########
#l.F..d...h..C.m#
#################
")

;!zprint {:format :skip}
(def example4 "
#############
#DcBa.#.GhKl#
#.###@#@#I###
#e#d#####j#k#
###C#@#@###J#
#fEbA.#.FgHi#
#############
")

(def input1 (slurp "resources/day18.txt"))

(def input2 (slurp "resources/day18b.txt"))

(defn ->grid
  [s]
  (let [lines (str/split-lines (str/trim s))]
    (->> (for [[line y] (map vector lines (range))
               [cell x] (map vector line (range))]
           [[x y] cell])
         (into {}))))

(def wall? (partial = \#))

(def key? (set "abcdefghijklmnopqrstuvwxyz"))

(def door? (set "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))

(def origin? (partial = \@))

(defn ->graph
  [grid]
  (let [nodes (->> grid
                   (remove (comp wall? second))
                   (map (fn [[xy c]]
                          (vector
                            xy
                            (cond-> {}
                              (key? c) (assoc :key c)
                              (door? c) (assoc :door (Character/toLowerCase c))
                              (origin? c) (assoc :origin c))))))
        edges (->> grid
                   (remove (comp wall? second))
                   (mapcat (fn [[[x y]]]
                             (vector [[x y] [(inc x) y] 1]
                                     [[x y] [x (inc y)] 1])))
                   (remove (comp wall? grid second)))]
    (-> (uber/graph)
        (uber/add-nodes-with-attrs* nodes)
        (uber/add-edges* edges))))

(defn paths-to-keys
  "What keys can we reach given a start node and keyring?"
  [g keyring start]
  (let [can-pass? (fn [n]
                    (let [door (uber/attr g n :door)]
                      (or (not door) (keyring door))))
        interesting-key? (fn [n]
                           (let [key (uber/attr g n :key)]
                             (and key (not (keyring key)))))
        from-start (alg/shortest-path g
                                      {:start-node start
                                       :node-filter can-pass?
                                       :cost-attr :weight})
        reachable-keys (->> (alg/all-destinations from-start)
                            (filter interesting-key?))]
    (map (partial alg/path-to from-start) reachable-keys)))

(defn explore-
  [best {:keys [g need keyring starts] steps-here :steps :as state}]
  ;(when (<= (count keyring) 5) (println keyring need))
  (cond-let
    (zero? need) steps-here
    :let [best-here (@best [starts keyring])]
    (and (not (nil? best-here)) (>= steps-here best-here)) Long/MAX_VALUE
    :else
      (do (swap! best assoc [starts keyring] steps-here)
          (apply min
            (for [[i start] (map-indexed vector starts)
                  p (sort-by alg/cost-of-path (paths-to-keys g keyring start))
                  :let [n (alg/end-of-path p)]]
              (explore- best
                        (assoc state
                          :steps (+ steps-here (alg/cost-of-path p))
                          :keyring (conj keyring (uber/attr g n :key))
                          :need (dec need)
                          :starts (assoc starts i n))))))))

(defn explore
  [& args]
  (apply explore- (atom {}) args))

(defn shortest
  [input]
  (let [grid (->grid input)
        g (-> (->graph grid) trim-non-path-nodes join-hallways)
        origins (->> (uber/nodes g) (filterv #(uber/attr g % :origin)))
        need (->> grid vals (filter key?) count)
        state {:g g :keyring #{} :starts origins :steps 0 :need need}]
    (explore state)))

(defn part1
  []
  (shortest input1))

(defn part2
  []
  (shortest input2))
