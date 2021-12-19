(ns aoc2021.day13
  (:require [clojure.string :as str]))

(defn merge-lines [l1 l2]
  (mapv #(or (re-matches #"#" %) (re-matches #"#" %2) ".") l1 l2))

(defn fold-horizontal [y m]
  (let [p? (zero? (mod (count m) 2))
        n (if p? y (dec y))
        bottom (reverse (take-last n m))
        top (take (if p? y (dec y)) m)]
    (mapv merge-lines top bottom)))

(defn fold-vertical [x m]
  (let [p? (zero? (mod (count (first m)) 2))
        n (if p? x (dec x))
        left (mapv #(take n %) m)
        right (mapv #(reverse (take-last n %)) m)]
    (mapv merge-lines right left)))

(defn fold [paper [direction point]]
  (case direction
    "y" (fold-horizontal point paper)
    "x" (fold-vertical point paper)))

(let [[paper folds] (str/split (slurp "resources/2021/day13") #"\n\n")
      folds (->> (str/split-lines folds)
                 (map #(str/split % #" "))
                 (map last)
                 (map #(str/split % #"="))
                 (map (fn [[x-or-y mag]] (vector x-or-y (inc (read-string mag))))))
      paper (->> (str/split-lines paper)
                 (map #(str/split % #","))
                 ((juxt #(map first %) #(map second %)))
                 (map #(map read-string %)))
      [maxx maxy] (map (comp dec #(* 2 %) second) (take 2 folds))
      paper (->> (take maxy (repeat (take maxx (repeat "."))))
                 (mapv vec)
                 ((fn [dot-matrix] (reduce #(assoc-in % %2 "#")
                                          dot-matrix
                                          (map vector (second paper) (first paper))))))]
  (->> (fold paper (first folds))
       flatten
       (filter #(= "#" %))
       count) ;; 1st star
  (->>
   (reduce fold paper folds)
   (map println))) ;; 2nd star
