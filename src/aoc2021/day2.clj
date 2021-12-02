(ns aoc2021.day2
  (:require [common.util :refer [parse-lines]]))

(def input (parse-lines "resources/2021/day2"))

(defn navigate [[x y aim] command]
  (let [[_ direction magnitude-s] (re-find #"(\w+) (\d+)" command)
        magnitude (Integer/parseInt magnitude-s)]
    (if aim
      (case direction
        "forward" [(+ x magnitude) (+ y (* aim magnitude)) aim]
        "up" [x y (- aim magnitude)]
        "down" [x y (+ aim magnitude)])
      (case direction
        "forward" [(+ x magnitude) y]
        "up" [x (- y magnitude)]
        "down" [x (+ y magnitude)]))))


(->> input ;; 1st star
     (reduce navigate [0 0])
     (take 2)
     (apply *))

(->> input ;; 2nd star
     (reduce navigate [0 0 0])
     (take 2)
     (apply *))
