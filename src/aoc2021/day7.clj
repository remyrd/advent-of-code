(ns aoc2021.day7
  (:require [clojure.string :as str]))

(def input (map read-string (-> "resources/2021/day7"
                                slurp
                                (str/split #","))))

(defn calc-price-1 [n col] ;; 1st star
  (reduce #(+ % (Math/abs (- n %2))) 0 col))

(defn calc-price-2 [n col] ;; 2nd star
  (reduce #(+ % (apply + (range (inc (Math/abs (- n %2)))))) 0 col))

;; Brute force... ugly and unnecessary
;; There's a unique minimum
;; It is close to the avg / median
;; Should calculate from there
(->>
 (range (count input))
 (map #(calc-price-2 % input)) ;; Change calc-price-1/2 for */**
 (partition 2 1)
 (partition-by #(apply > %))
 (map ffirst)
 (apply min))
