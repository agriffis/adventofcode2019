(ns adventofcode2019.graph
  (:require [ubergraph.core :as uber]
            [ubergraph.alg :as alg]))

(def not-empty? (complement empty?))

(def not-nil? (complement nil?))

(defn- has-attrs? [g n] (not-empty? (uber/attrs g n)))

(defn trim-non-path-nodes
  "Remove all the nodes that aren't part of a shortest-path between
  interesting nodes."
  ([g] (trim-non-path-nodes g has-attrs?))
  ([g interesting?]
   (let [interesting-nodes (->> (uber/nodes g)
                                (filter (partial interesting? g)))
         paths (for [start interesting-nodes
                     :let [from-start (alg/shortest-path g {:start-node start})]
                     end interesting-nodes
                     :when (not= start end)]
                 (alg/path-to from-start end))
         nodes-on-paths (set (mapcat alg/nodes-in-path paths))
         kill-nodes (->> (uber/nodes g) (remove nodes-on-paths))]
     (uber/remove-nodes* g kill-nodes))))

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
  "Replace all uninteresting hallway nodes by spanning edges."
  ([g] (join-hallways g has-attrs?))
  ([g interesting?]
   (->> (uber/nodes g)
        (reduce
          (fn [g node]
            (let [[e1 e2 e3] (uber/out-edges g node)]
              (if (or (interesting? g node)
                      (nil? e2) ; dead end
                      (not-nil? e3) ; not a hallway
                    )
                g
                (bridge-node g node))))
          g))))
