(ns aoc2021.day10
  (:require [common.util :refer [parse-lines parse-example]]))

(def input (parse-lines "resources/2021/day10"))

(def char->points-illegal {\) 3 \] 57 \} 1197 \> 25137})

(def char->points-incomplete {\) 1 \] 2 \} 3 \> 4})

(def open->closing
  {\( \) \[ \] \{ \} \< \>})

(defn find-illegal [line]
  (reduce (fn [stack c]
            (if (open->closing c)
              (conj stack c)
              (if (= (open->closing (last stack)) c)
                (vec (butlast stack))
                (reduced c))))
          [] line))

(defn calc-score [total c]
  (+ (* 5 total) (-> c open->closing char->points-incomplete)))

(->> input
     (map find-illegal)
     (remove vector?)
     (map char->points-illegal)
     (apply +)) ;; 1st star

(->> input
     (map find-illegal)
     (filter vector?)
     (map reverse)
     (map #(reduce calc-score 0 %))
     sort
     (#(nth % (quot (count %) 2)))) ;; 2nd star
