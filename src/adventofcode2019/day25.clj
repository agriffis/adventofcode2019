(ns adventofcode2019.day25
  (:require [clojure.math.combinatorics :refer [subsets]]
            [clojure.set :as set]
            [clojure.string :as str]
            [adventofcode2019.contrib :refer [cond-let]]
            [adventofcode2019.intcode :refer [->core run-ascii]]))

(def program (slurp "resources/day25.txt"))

(def abbrs
  {"n" "north"
   "e" "east"
   "s" "south"
   "w" "west"
   "u" "undo"
   "t" "take"
   "d" "drop"
   "q" "quit"})

(defn expand-abbrs
  [{:keys [input] :as core}]
  (let [[cmd & args] (str/split input #"\s+")]
    (assoc core :input (str/join " " (cons (get abbrs cmd cmd) args)))))

(defn inv
  [core]
  (let [{:keys [output]} (run-ascii core "inv")]
    (map second (re-seq #"\n- (.*)" output))))

(defn divest
  [core things]
  (reduce #(run-ascii %1 (str "drop " %2)) core things))

(defn acquire
  [core things]
  (reduce #(run-ascii %1 (str "take " %2)) core things))

(defn bypass-security
  [{:keys [input] :as core}]
  (if (or (not (#{"north" "east" "south" "west"} input))
          (not (-> (run-ascii core) :output (.contains "Alert!"))))
    core
    (do (println "ATTEMPTING TO BYPASS SECURITY")
        (let [stuff (inv core)
              unladen (divest core stuff)
              laden (first (for [carry (subsets stuff)
                                 :let [laden (-> (acquire unladen carry)
                                                 (assoc :input input))
                                       {:keys [output]} (run-ascii laden)]
                                 :when (not (.contains output "Alert!"))]
                             laden))]
          (or laden core)))))

(defn items-here
  [{:keys [output]}]
  (when-let [s (re-find #"\nItems here:\n(?:-.*\n)+" output)]
    (map second (re-seq #"\n- (.*)" s))))

(defn take-all
  [{:keys [input output] :as core}]
  (if (not= "take" input)
    core
    (-> core (acquire (items-here core)) (assoc :input "inv"))))

(def tools [take-all bypass-security expand-abbrs])

(defn prompt
  [{:keys [output] :as core}]
  (println output)
  ((apply comp tools) (assoc core :input (read-line))))

(defonce saved [(run-ascii (->core program))])

(defn play
  []
  (loop [cores saved]
    (let [{:keys [input] :as core} (-> (first cores) prompt)]
      (cond (= input "quit") nil
            (= input "undo")
              (if (second cores) (recur (rest cores)) (recur cores))
            :else (do (def saved (cons (run-ascii core) cores))
                      (recur saved))))))
