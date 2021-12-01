(ns aoc2020.day10
  (:require [clojure.string :as string]))

(defn diffs [coll]
  (map -
       (concat coll [(+ 3 (last coll))])
       (cons 0 coll)))

(defn power [b e]
  (apply * (repeat e b)))

(defn changes [n]
  (let [bits (power 2 n)]
    (if (< n 3)
      bits
      (- bits (apply + (map (partial power 2) (range (- n 2))))))))

(comment  (->> "resources/2020/day10"
             slurp
             string/split-lines
             (map #(Integer/parseInt %))
             sort
             diffs
             (partition-by identity)
             (remove #(= 3 (first %)))
             (map rest)
             (map #(changes (count %)))
             (apply *)
             ))
