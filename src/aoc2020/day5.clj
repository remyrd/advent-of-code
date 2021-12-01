(ns aoc2020.day5
  (:require [clojure.string :as string]))

(defn c->int [i c]
  (case c
    (\B \R) (apply * (repeat i 2))
    0))

(defn seat-id [seat]
  (let [[col row] (->> seat
                     reverse
                     (split-at 3)
                     (map (partial map-indexed c->int))
                     (map (partial apply +)))]
    (+ col (* 8 row))))

(defn find-seat [seats]
  (for [seat+1 seats
        :let [seat (dec seat+1)
              seat-1 (dec seat)]
        :when (and (nil? (some #(= seat %) seats))
                   (some #(= seat-1 %) seats))]
    seat))

(->> "resources/2020/day5"
   slurp
   (string/split-lines)
   (map seat-id)
   find-seat)
