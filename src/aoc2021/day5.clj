(ns aoc2021.day5
  (:require [common.util :refer [parse-lines]]))

(def input (parse-lines "resources/2021/day5"))

(defn parse-line [text-line]
  (mapv read-string (re-seq #"\d+" text-line)))

(defn vertical-or-horizontal? [[x1 y1 x2 y2]]
  (or (= x1 x2) (= y1 y2)))

(defn add-points [t points]
  (if (empty? points)
      t
      (let [[p & ps] points]
        (recur (update-in t p #(if % (inc %) 1))
               ps))))

(defn make-range [point1 point2]
  (cond
    (= point1 point2) (repeat point1)
    (< point1 point2) (range point1 (inc point2))
    (> point1 point2) (reverse (range point2 (inc point1)))))

;; matrix represented as a tree of x->y
(defn build-matrix [lines]
  (reduce (fn [t [x1 y1 x2 y2]]
            (add-points t (map vector
                               (make-range x1 x2)
                               (make-range y1 y2))))
          {} lines))

(defn get-result [m]
  (->> m vals (map vals) flatten (filter #(< 1 %)) count))

(->> input
     (map parse-line)
     (filter vertical-or-horizontal?)
     build-matrix
     get-result)

(->> input
     (map parse-line)
     build-matrix
     get-result)
