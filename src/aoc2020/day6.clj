(ns aoc2020.day6
  (:require [clojure.string :as string]
            [clojure.set :as s]))


(->> "resources/2020/day6"
   slurp
   string/split-lines
   (map set)
   (partition-by #(= % #{}))
   (remove #(= (first %) #{}))
   (map #(apply s/intersection %))
   (map count)
   (apply +))
