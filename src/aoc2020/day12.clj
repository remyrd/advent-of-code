(ns aoc2020.day12
  (:require [clojure.string :as string]))

(def angle->nwse {0 \E, 90 \N, 180 \W, 270 \S})
(def nwse->x {\E 1,\N 0,\W -1,\S 0})
(def nwse->y {\E 0,\N 1,\W 0,\S -1})

(defn waypoint [[x y wx wy] [dir mag]]
  (let [left (fn [[a b]] [(* -1 b) a])
        right (fn [[a b]] [b (* -1 a)])]
    (case dir
      \F [(+ x (* mag wx))
          (+ y (* mag wy))
          wx
          wy]
      \L (concat [x y] (last (take (inc (/ mag 90))
                                   (iterate left [wx wy]))))
      \R (concat [x y] (last (take (inc (/ mag 90))
                                   (iterate right [wx wy]))))
      (\N \W \S \E) [x
                     y
                     (+ wx (* mag (nwse->x dir)))
                     (+ wy (* mag (nwse->y dir)))])))

(defn walk-direction [[x y a] [dir mag]]
  (case dir
     \F [(+ x (* mag (nwse->x (angle->nwse a))))
         (+ y (* mag (nwse->y (angle->nwse a))))
         a]
     \L [x y (mod (+ a mag) 360)]
     \R [x y (mod (- a mag) 360)]
     (\N \W \S \E) [(+ x (* mag (nwse->x dir)))
                    (+ y (* mag (nwse->y dir)))
                    a]))

(comment  (->> "resources/2020/day12"
             slurp
             string/split-lines
             (map (partial re-seq #"([A-Z])(\d+)"))
             (map first)
             (map (juxt (comp first second) #(Integer/parseInt (nth % 2))))
             (reduce waypoint [0 0 10 1])
             (take 2)
             (map #(Math/abs %))
             (apply +)))

