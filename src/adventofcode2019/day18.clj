(ns adventofcode2019.day18
  (:require [adventofcode2019.contrib :refer [cond-let]]
            [clojure.core.cache :as cache]
            [clojure.math.numeric-tower :as math]
            [clojure.set :as set]
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
  [{:keys [g keyring start]}]
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
  [best {:keys [g need steps keyring start] :as state}]
  (when (<= (count keyring) 5) (println keyring need))
  (if (zero? need)
    steps
    (let [b (@best [start keyring])]
      (when (or (nil? b) (> b steps))
        (do (swap! best assoc [start keyring] steps)
            (let [ps (paths-to-keys state)]
              (apply min
                Long/MAX_VALUE
                (for [p (sort-by alg/cost-of-path ps)
                      :let [add-steps (alg/cost-of-path p)
                            node (alg/end-of-path p)
                            key (uber/attr g node :key)
                            steps (explore- best
                                            (assoc state
                                              :steps (+ steps add-steps)
                                              :keyring (conj keyring key)
                                              :need (dec need)
                                              :start node))]
                      :when steps]
                  steps))))))))

(defn explorer
  []
  (partial explore- (atom {})))

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
        state {:g g :keyring #{} :start origin :steps 0 :need need}
        explore (explorer)]
    (uber/pprint g)
    (time (explore state))))
