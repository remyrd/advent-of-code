(ns aoc2020.day2
  (:require [clojure.string :refer [split]]))

;; 15-19 k: kkkkkkkkkkkkzkkkkkkk
(defn parseline [l]
  (->> l
       (#(split % #" "))
       ((juxt (comp #(map (fn [x] (Integer/parseInt x)) %) #(split % #"-") first)
              #(map (partial re-find #"[a-z]+") %)))
       flatten
       (keep identity)))


(defn parsefile [f]
  (->> f
       slurp
       (#(split % #"\n"))))

(defn valid1? [l]
  (let [[mi ma c s] l
        cnt (count (filter #(= (first c) %) s))]
    (and (<= cnt ma)
         (>= cnt mi))))

(defn valid2? [l]
  (let [[mi ma c s] l]
    (not= (= (nth s (dec mi)) (first c))
          (= (nth s (dec ma)) (first c)))))

(defn day2 [f]
  (let [lines (->> f parsefile (map parseline))]
    (count (filter valid2? lines))))


(comment
  (valid2?  (parseline "15-19 k: kkkkkkkkkkkkkkkzkkkkkkk"))
  (day2 "resources/day2"))
