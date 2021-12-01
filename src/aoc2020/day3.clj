(ns aoc2020.day3
  (:require [clojure.java.io :as io]))

(defn hit-tree [[total pos skip xstep ystep] line]
  (let [ev? (zero? (mod skip ystep))
        newpos (if ev?
                 (mod (+ pos xstep) (count line))
                 pos)]
    [(if (and ev? (= \# (nth line pos)))
       (inc total)
       total)
     newpos
     (inc skip)
     xstep
     ystep]))

(defn ex1 [filename [xstep ystep]]
  (with-open [rdr (io/reader filename)]
    (->> rdr
       line-seq
       (reduce hit-tree [0 0 0 xstep ystep])
       first)))


(comment (ex1 "resources/2020/day3" [3 1])
         (->> [[1 1] [3 1] [5 1] [7 1] [1 2]]
            (map (partial ex1 "resources/2020/day3"))
            (apply *))
         (hit-tree [0 9] "######...."))
