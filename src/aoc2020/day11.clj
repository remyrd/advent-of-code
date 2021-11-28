(ns aoc2020.day11
  (:require [clojure.string :as string]))

(defn walk-direction [x y dirx diry maxx maxy world]
  (let [newx (+ x dirx)
        newy (+ y diry)]
    (when (and (<= 0 newx maxx)
               (<= 0 newy maxy))
      (case (nth (nth world newy) newx)
        \. (walk-direction newx newy dirx diry maxx maxy world)
        \# \#
        \L \L))))

(defn count-visible [x y world]
  (let [maxx (dec (count (first world)))
        maxy (dec (count world))]
    (->>  (for [dirx [-1 0 1]
              diry [-1 0 1]
              :when (or (not= dirx 0)
                        (not= diry 0))]
          (walk-direction x y dirx diry maxx maxy world))
        (filter #(= \# %))
        count)))

(defn count-adjacent [x y world]
  (->>  (for [xs (filter #(<= 0 % (dec (count (first world)))) (range (dec x) (+ 2 x)))
              ys (filter #(<= 0 % (dec (count world))) (range (dec y) (+ 2 y)))
              :when (or (not= xs x)
                        (not= ys y))]
          (nth (nth world ys) xs))
        (filter #(= \# %))
        count))

(defn evolve [x y cell world]
  (if (= cell \.)
    cell
    (let [alive-neighbors (count-visible x y world)]
      (case cell
        \# (if (>= alive-neighbors 5)
             \L
             \#)
        \L (if (zero? alive-neighbors)
             \#
             \L)))))

(defn map-world [world]
  (map-indexed (fn [y line]
                 (map-indexed (fn [x cell]
                                (evolve x y cell world))
                              line))
               world))

(defn play-game [lines]
  (loop [world lines]
    (let [new-world (map-world world)]
      (if (= new-world world)
        (->> new-world
           (map (fn [line] (filter #(= % \#) line)))
           (map count)
           (apply +))
        (recur new-world)))))

(comment  (->> "resources/day11"
             slurp
             string/split-lines
             play-game))

