(ns aoc2020.day24
  (:require [clojure.string :as string]
            [clojure.set :refer [intersection]]))

(defn next-tile [[x y] step]
  (case step
    "ne" [(inc x) (inc y)]
    "e" [(+ x 2) y]
    "se" [(inc x) (dec y)]
    "sw" [(dec x) (dec y)]
    "w" [(- x 2) y]
    "nw" [(dec x) (inc y)]
    [x y]))

(defn init-flip-tiles [input]
  (->> input
       string/split-lines
       (map (comp (partial reduce next-tile [0 0])
               (partial map second)
               (partial re-seq #"([ns]?[ew])")))
       frequencies))

(defn fetch-all-neighbors [xy]
  (set (map (partial next-tile xy) ["nw" "w" "sw" "se" "e" "ne"])))

(defn next-day [blacks]
  (for [tile (set (concat blacks (mapcat fetch-all-neighbors blacks)))
        :let [adjacent (count (intersection blacks (fetch-all-neighbors tile)))]
        :when  (if (contains? blacks tile)
                 (< 0 adjacent 3)
                 (= 2 adjacent))]
    tile))

(comment
  (->> (slurp "resources/2020/day24") ;; part 2
       init-flip-tiles
       (filter #(= 1 (mod (val %) 2)))
       (map first)
       set
       (iterate (comp set next-day))
       (take 101)
       last
       count)
  (->> (slurp "resources/2020/day24") ;; part 1
       init-flip-tiles
       vals
       (group-by #(mod % 2))
       (#(get % 1))
       count))
