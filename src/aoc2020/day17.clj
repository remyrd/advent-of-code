(ns aoc2020.day17
  (:require [clojure.string :as string]))

(defn get-ranges [m]
  [(range (apply min (keys m)) (inc (apply max (keys m))))
   (range (apply min (keys (get m 0))) (inc (apply max (keys (get m 0)))))
   (range (apply min (keys (get-in m [0 0]))) (inc (apply max (keys (get-in m [0 0])))))
   (range (apply min (keys (get-in m [0 0 0]))) (inc (apply max (keys (get-in m [0 0 0])))))])

(defn count-active [m]
  (let [[wrange zrange yrange xrange] (get-ranges m)]
    (count 
     (for [w wrange
           z zrange
           y yrange
           x xrange
           :let [c (get-in m [w z y x])]
           :when (= \# c)]
       c))))

(defn count-active-neighbors [w z y x current]
  (->> (for [ws (range (dec w) (+ 2 w))
             zs (range (dec z) (+ 2 z))
             ys (range (dec y) (+ 2 y))
             xs (range (dec x) (+ 2 x))
             :let [c (get-in current [ws zs ys xs] \.)]
             :when (or (not= w ws) (not= z zs) (not= y ys) (not= x xs))]
         c)
       (filter (partial = \#))
       count))

(defn boot [m]
  (let [ranges (get-ranges m)
        cube-size (apply * (map (comp (partial + 2) count) ranges))
        [minw minz miny minx] (map (comp dec (partial apply min)) ranges)
        [maxw maxz maxy maxx] (map (comp inc (partial apply max)) ranges)]
    (loop [w minw
           z minz
           y miny
           x minx
           c 0
           new-m {}]
      (let [neighbors (count-active-neighbors w z y x m)
            next-x (if (= x maxx) minx (inc x))
            next-y (if (= next-x minx)
                     (if (= y maxy) miny (inc y))
                     y)
            next-z (if (and (= next-y miny) (= next-x minx))
                     (if (= z maxz) minz (inc z))
                     z)
            next-w (if (and (= next-z minz) (= next-y miny) (= next-x minx))
                     (if (= w maxw) minw (inc w))
                     w)]
        (if (= c cube-size)
          new-m
          (recur next-w next-z next-y next-x (inc c)
                 (assoc-in new-m [w z y x] (case (get-in m [w z y x] \.)
                                             \. (if (= 3 neighbors) \# \.)
                                             \# (if (< 1 neighbors 4) \# \.)))))))))

(def example ".#.
..#
###")

(comment
  (->>
   "resources/2020/day17"
   slurp
   ;; example
   string/split-lines
   (map-indexed #(vector % (into {} (map-indexed hash-map %2))))
   vec
   (into {})
   (assoc {} 0)
   (assoc {} 0)
   (iterate boot)
   (take 7)
   last
   count-active
   ))
