(ns aoc2021.day9
  (:require [common.util :refer [parse-lines]]
            [clojure.set :as s]))


(def input (->> "resources/2021/day9"
                parse-lines
                (mapv (partial mapv #(- (int %) 48)))))

(defn find-pits [matx]
  (filter identity
          (for [[y x-coll] (map-indexed vector matx)
                [x v] (map-indexed vector x-coll)
                :let [a (if (zero? y) 10 (get-in matx [(dec y) x])) ;; up
                      b (if (= x (dec (count x-coll))) 10 (get-in matx [y (inc x)])) ;; right
                      c (if (= y (dec (count matx))) 10 (get-in matx [(inc y) x])) ;; down
                      d (if (zero? x) 10 (get-in matx [y (dec x)])) ;; left
                      ]]
            (when (empty? (remove #(< v %) [a b c d])) [v [y x]]))))

(defn walk-basin [matx [y x] limitx limity visited]
  (if (or (contains? visited [y x])
          (= 9 (get-in matx [y x])))
    visited
    (cond->> visited
      true (s/union #{[y x]})
      (< 0 y) (walk-basin matx [(dec y) x] limitx limity) ;; up
      (< x limitx) (walk-basin matx [y (inc x)] limitx limity) ;; right
      (< y limity) (walk-basin matx [(inc y) x] limitx limity) ;; down
      (< 0 x) (walk-basin matx [y (dec x)] limitx limity) ;; left
      )))

(defn setup-walk [matx limitx limity pits]
    (map walk-basin (repeat matx) pits (repeat limitx) (repeat limity) (repeat #{})))

(->> input
     find-pits
     (map first)
     (map inc)
     (apply +)) ;; 1st star

(->> input
     (#(setup-walk %
                   (dec (count (first %)))
                   (dec (count %))
                   (map second (find-pits %))
                   ))
     (map count)
     sort
     (take-last 3)
     (apply *)) ;; 2nd star
