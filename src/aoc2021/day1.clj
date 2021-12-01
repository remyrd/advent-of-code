(ns aoc2021.day1
  (:require [clojure.string :as str]))

(def input (->> "resources/2021/day1"
                slurp
                str/split-lines
                (map #(Integer/parseInt %))))

(defn sweep-floor [coll]
  (reduce (fn [{:keys [increases current] :as acc} x]
            (assoc acc
                   :current x
                   :increases (if (< current x)
                                (inc increases)
                                increases)))
          {:increases 0
           :current (first coll)}
          (rest coll)))

(sweep-floor input) ;; 1st star
(sweep-floor (map #(apply + %) (partition 3 1 input))) ;; 2nd star
