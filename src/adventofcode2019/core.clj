(ns adventofcode2019.core
  (:gen-class)
  (:require [adventofcode2019.day13 :refer [program play draw]]
            [adventofcode2019.intcode :refer [->core]]))

(def clear (str \u001b "[2J"))

(def home (str \u001b "[H"))

(defn -main
  [& args]
  (print clear)
  (doseq [[grid score] (-> program ->core (assoc-in [:mem 0] 2) play)]
    (println (str home score "\n" (draw grid)))
    (Thread/sleep 1)))
