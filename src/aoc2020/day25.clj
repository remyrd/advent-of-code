(ns aoc2020.day25
  (:require [clojure.string :as string]))

(defn calc [subject]
  (iterate #(mod (* % subject) 20201227) 1))

(defn get-loops [pk subject]
  (count (take-while (partial not= pk) (calc subject))))

(defn get-secret [loops subject]
  (last (take (inc loops) (calc subject))))

(comment
  (->> (slurp "resources/day25")
       string/split-lines
       (map #(Integer/parseInt %))
       (apply #(get-secret (get-loops % 7) %2))))
