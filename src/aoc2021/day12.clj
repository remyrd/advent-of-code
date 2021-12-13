(ns aoc2021.day12
  (:require [common.util :refer [parse-lines]]
            [clojure.set :as set]
            [clojure.string :as str]))

(defn add-destination [old new]
  (if old
    (set/union old #{new})
    #{new}))

(defn build-cave-map [m [node1 node2]]
  (-> m
      (update node1 add-destination node2)
      (update node2 add-destination node1)))

(def input (->>
            "resources/2021/day12"
            parse-lines
            (map #(str/split % #"\-"))))

(def is-small? (partial re-matches #"[a-z]+"))

(defn explore-node [current-node path forbidden cave-map]
  (case current-node
    "end" path
    (let [forbidden' (if (is-small? current-node)
                      (set/union forbidden #{current-node})
                      forbidden)
          next-nodes (remove forbidden (get cave-map current-node))
          path' (conj path current-node)]
      (when (not-empty next-nodes)
        (for [n next-nodes]
          (explore-node n path' forbidden' cave-map))))))

(defn generate-small-node-pass-count [cave-map]
  (let [small-nodes (filter (comp #(when (and (not= "end" %) (not= "start" %)) %) is-small?) (keys cave-map))]
    (map (fn [m k] (-> m
                      (update k inc)
                      (assoc "start" 1)
                      (assoc "end" 1)))
           (repeat (zipmap small-nodes (repeat 1)))
           small-nodes)))


(generate-small-node-pass-count {"start" nil "abc" 12 "wef" 12 "AS" 12})

(defn explore-node-multi-pass [current-node path forbidden pass-count cave-map]
  (case current-node
    "end" path
    (let [pass-count' (if (is-small? current-node)
                        (update pass-count current-node dec)
                        pass-count)
          forbidden' (if (and (is-small? current-node) (< (get pass-count' current-node) 1))
                       (set/union forbidden #{current-node})
                       forbidden)
          next-nodes (remove forbidden' (get cave-map current-node))
          path' (conj path current-node)]
      (when (not-empty next-nodes)
        (for [n next-nodes]
          (explore-node-multi-pass n path' forbidden' pass-count' cave-map))))))

(->> input
     (reduce build-cave-map {})
     (explore-node "start" [] #{})
     flatten
     (filter #(= "start" %))
     count) ;; 1st star

(let [cave-map (reduce build-cave-map {} input)
      pass-count (generate-small-node-pass-count cave-map)]
  (->>
   (map (partial explore-node-multi-pass "start" [] #{}) pass-count (repeat cave-map))
   flatten
   (filter identity)
   (partition-by #(= "start" %))
   set
   count
   dec)) ;; 2nd star
