(ns advent-of-code.day6
  (:require [clojure.string :as string]
            [clojure.set :as s]))


(->> "resources/day6"
   slurp
   string/split-lines
   (take 10)
   (map set)
   (partition-by #(= % #{}))
   (remove #(= (first %) #{}))
   (apply s/intersection))
