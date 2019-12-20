(ns adventofcode2019.day18
  (:require [adventofcode2019.contrib :refer [cond-let]]
            [clojure.string :as str]
            [ubergraph.core :as uber]
            [ubergraph.alg :as alg]))

(defn ->grid
  [s]
  (let [lines (str/split-lines s)]
    (->> (for [[line y] (map vector lines (range))
               [cell x] (map vector line (range))]
           [[x y] cell])
         (into {}))))

(def wall? (partial = \#))

(def not-wall? (complement wall?))

(def key? (set "abcdefghijklmnopqrstuvwxyz"))

(def door? (set "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))

(def origin? (partial = \@))

(defn ->graph
  [grid]
  (let [nodes (->> grid
                   (filter (comp not-wall? second))
                   (map (fn [[xy c]]
                          (vector
                            xy
                            (cond-> {}
                              (key? c) (assoc :key c)
                              (door? c) (assoc :door (Character/toLowerCase c))
                              (origin? c) (assoc :origin c))))))
        edges (->> grid
                   (filter (comp not-wall? second))
                   (mapcat (fn [[[x y]]]
                             (vector [[x y] [(inc x) y] 1]
                                     [[x y] [x (inc y)] 1])))
                   (filter (comp not-wall? grid second)))]
    (-> (uber/graph)
        (uber/add-nodes-with-attrs* nodes)
        (uber/add-edges* edges))))

(def not-nil? (complement nil?))

(defn trim-non-path-nodes
  "Remove all the nodes that aren't part of a shortest-path between two
  interesting nodes."
  [g]
  (let [interesting-nodes (->> (uber/nodes g)
                               (remove #(empty? (uber/attrs g %))))
        paths (for [start interesting-nodes
                    :let [from-start (alg/shortest-path g {:start-node start})]
                    end interesting-nodes
                    :when (not= start end)]
                (alg/path-to from-start end))
        nodes-on-paths (set (mapcat alg/nodes-in-path paths))
        kill-nodes (->> (uber/nodes g) (remove nodes-on-paths))]
    (uber/remove-nodes* g kill-nodes)))

(defn bridge-node
  "Replace a hallway node by a spanning edge."
  [g node]
  (let [es (uber/out-edges g node)
        [e1 e2] es]
    (assert (= 2 (count es)))
    (-> g
        (uber/add-edges [(uber/dest e1) (uber/dest e2)
                         (+ (uber/weight g e1) (uber/weight g e2))])
        (uber/remove-nodes node))))

(defn join-hallways
  "Replace all the hallway nodes by spanning edges."
  ([g] (->> (uber/nodes g) (reduce join-hallways g)))
  ([g node]
   (let [[e1 e2 e3] (uber/out-edges g node)]
     (if (or (uber/attr g node :origin)
             (uber/attr g node :key)
             (uber/attr g node :door)
             (nil? e2) ; dead end
             (not-nil? e3) ; not a hallway
           )
       g
       (bridge-node g node)))))

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
  [best {:keys [g need steps keyring starts] :as state}]
  (when (<= (count keyring) 5) (println keyring need))
  (cond-let
    (zero? need) steps
    :let [[best-before best-after] (get @best [starts keyring] [])]
    (not-nil? best-before)
      (do (when (< steps best-before)
            (swap! best assoc [starts keyring] [steps best-after]))
          best-after)
    :else
      (let [steps-after
              (apply min
                Long/MAX_VALUE
                (for [[i start] (map-indexed vector starts)
                      :let [ps (paths-to-keys g keyring start)]
                      p (sort-by alg/cost-of-path ps)
                      :let [node (alg/end-of-path p)
                            steps (explore-
                                    best
                                    (assoc state
                                      :steps (+ steps (alg/cost-of-path p))
                                      :keyring (conj keyring
                                                     (uber/attr g node :key))
                                      :need (dec need)
                                      :starts (assoc starts i node)))]
                      :when steps]
                  steps))]
        (swap! best assoc [starts keyring] [steps steps-after])
        steps-after)))

(defn explore
  [& args]
  (apply explore- (atom {}) args))

(def example
  "########################
#f.D.E.e.C.b.A.@.a.B.c.#
######################.#
#d.....................#
########################")

(def example2
  "#################
#i.G..c...e..H.p#
########.########
#j.A..b...f..D.o#
########@########
#k.E..a...g..B.n#
########.########
#l.F..d...h..C.m#
#################")

(def input (slurp "resources/day18.txt"))

(defn part1
  [input]
  (let [grid (->grid input)
        g (-> (->graph grid) trim-non-path-nodes join-hallways)
        origin (->> (uber/nodes g) (filter #(uber/attr g % :origin)) first)
        need (->> grid vals (filter key?) count)
        state {:g g :keyring #{} :starts [origin] :steps 0 :need need}]
    (uber/pprint g)
    (time (explore state))))

(def example3
  "#############
#DcBa.#.GhKl#
#.###@#@#I###
#e#d#####j#k#
###C#@#@###J#
#fEbA.#.FgHi#
#############")

(def input2 (slurp "resources/day18b.txt"))

(defn part2
  [input]
  (let [grid (->grid input)
        g (-> (->graph grid) trim-non-path-nodes join-hallways)
        origins (->> (uber/nodes g) (filterv #(uber/attr g % :origin)))
        need (->> grid vals (filter key?) count)
        state {:g g :keyring #{} :starts origins :steps 0 :need need}]
    (uber/pprint g)
    (time (explore state))))
