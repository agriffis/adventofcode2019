(ns adventofcode2019.day14
  (:require [clojure.math.numeric-tower :as math]
            [clojure.string :as str]
            [ubergraph.core :as uber]))

;;; ----------------------------------------------------------------------
;;; Part 1
;;; ----------------------------------------------------------------------

(defn ->component
  [s]
  (let [[amount chemical] (-> s str/trim (str/split #"\s+"))]
    [chemical (Long/parseLong amount)]))

(defn ->rule
  [s]
  (let [s (str/trim s)
        [lhs rhs] (str/split s #"\s*=>\s*")
        reactants (->> (str/split lhs #"\s*,\s*") (mapv ->component) (into {}))
        [product amount] (->component rhs)]
    {:rule s :reactants reactants :product product :yield amount}))

(defn ->rules
  [s]
  (->> (str/split-lines s) (mapv ->rule)))

(defn ->node
  [product]
  [product {:rem 0}])

(defn ->edges
  [{:keys [product yield reactants] :as rule}]
  (mapv (fn [[reactant cost]] (vector product reactant (assoc rule :cost cost)))
    reactants))

(defn ->graph
  [rules]
  (-> (apply uber/multidigraph
        (concat [["ORE" {:total 0}]]
                (map (comp ->node :product) rules)
                (mapcat ->edges rules)))))

(defn rule-edges
  [graph node]
  (->> (uber/find-edges graph {:src node})
       (group-by #(uber/attr graph % :rule))
       vals))

(defn update-attr
  [graph node-or-edge k f]
  (let [attrs (-> (uber/attrs graph node-or-edge) (update k f))]
    (uber/set-attrs graph node-or-edge attrs)))

(declare mine)

(defn next-multiple
  [m o]
  (* m (math/ceil (/ o m))))

(defn dig
  [bundles-count graph edge]
  (let [[product reactant {:keys [cost yield]}]
          ((juxt uber/src uber/dest (partial uber/attrs graph)) edge)
        need-reactant (* cost bundles-count)]
    (cond (zero? need-reactant) graph
          (= "ORE" reactant)
            (update-attr graph "ORE" :total #(+ % need-reactant))
          :else (mine graph reactant need-reactant))))

(def non-zero? (complement zero?))

(defn mine
  ([graph] (mine graph "FUEL" 1))
  ([graph node need]
   (->> (for [edges (rule-edges graph node)
              :let [[product {:keys [yield]}]
                      ((juxt uber/src (partial uber/attrs graph)) (first edges))
                    have (min need (uber/attr graph product :rem))
                    need (- need have)
                    produce (next-multiple yield need)
                    extra (- produce need)
                    adjust (- extra have)
                    bundles-count (/ produce yield)]]
          (cond-> (reduce (partial dig bundles-count) graph edges)
            (non-zero? adjust) (update-attr product :rem (partial + adjust))))
        ;(map #(do (uber/pprint %) %))
        (sort-by #(uber/attr % "ORE" :total))
        first)))

(defn ore
  [graph & args]
  (-> (apply mine graph args) (uber/attr "ORE" :total)))

(def input (slurp "resources/day14.txt"))

(def graph (-> input ->rules ->graph))

(defn part1
  []
  (ore graph))

;;; ----------------------------------------------------------------------
;;; Part 2
;;; ----------------------------------------------------------------------
;;;
;;; adventofcode2019.day14-test=> (ore graph "FUEL" 100)
;;; 53852480N
;;; adventofcode2019.day14-test=> (ore graph "FUEL" 200)
;;; 107527031N
;;; adventofcode2019.day14-test=> (ore graph "FUEL" 2000)
;;; 1073451638N
;;; adventofcode2019.day14-test=> (ore graph "FUEL" 20000)
;;; 10731268894N
;;; adventofcode2019.day14-test=> (ore graph "FUEL" 200000)
;;; 107311325177N
;;; adventofcode2019.day14-test=> (ore graph "FUEL" 2000000)
;;; 1073110188759N
;;; adventofcode2019.day14-test=> (ore graph "FUEL" 1900000)
;;; 1019454696786N
;;; adventofcode2019.day14-test=> (ore graph "FUEL" 1850000)
;;; 992627050624N
;;; adventofcode2019.day14-test=> (ore graph "FUEL" 1880000)
;;; 1008723690545N
;;; adventofcode2019.day14-test=> (ore graph "FUEL" 1860000)
;;; 997992491333N
;;; adventofcode2019.day14-test=> (ore graph "FUEL" 1870000)
;;; 1003358181079N
;;; adventofcode2019.day14-test=> (ore graph "FUEL" 1865000)
;;; 1000675251796N
;;; adventofcode2019.day14-test=> (ore graph "FUEL" 1861000)
;;; 998528999805N
;;; adventofcode2019.day14-test=> (ore graph "FUEL" 1862000)
;;; 999065410420N
;;; adventofcode2019.day14-test=> (ore graph "FUEL" 1863000)
;;; 999602311971N
;;; adventofcode2019.day14-test=> (ore graph "FUEL" 1863500)
;;; 999870424140N
;;; adventofcode2019.day14-test=> (ore graph "FUEL" 1863800)
;;; 1000031342985N
;;; adventofcode2019.day14-test=> (ore graph "FUEL" 1863700)
;;; 999977901563N
;;; adventofcode2019.day14-test=> (ore graph "FUEL" 1863750)
;;; 1000004357208N
;;; adventofcode2019.day14-test=> (ore graph "FUEL" 1863740)
;;; 999999183749N
;;; adventofcode2019.day14-test=> (ore graph "FUEL" 1863745)
;;; 1000001885543N
;;; adventofcode2019.day14-test=> (ore graph "FUEL" 1863744)
;;; 1000001355210N
;;; adventofcode2019.day14-test=> (ore graph "FUEL" 1863743)
;;; 1000000732654N
;;; adventofcode2019.day14-test=> (ore graph "FUEL" 1863742)
;;; 1000000345469N
;;; adventofcode2019.day14-test=> (ore graph "FUEL" 1863741)
;;; 999999809355N
