(ns aoc2021.day6
  (:require [clojure.string :as str]))

(def input (-> "resources/2021/day6"
                slurp
                (str/split #",")
                (#(map read-string %))))

(defn reproduction-date->nfish
  "Takes the input and maps how many fish are born each day"
  [input]
  (reduce #(assoc % %2
                  (if-let [n (get % %2)]
                    (inc n)
                    1))
          {} input))

(defn model-new-fish
  "Given a day, figures out and adds to the map the amount of fish that
  will reproduce on a future date"
  [repr-date->fish day]
  (let [fish-born-in-8-days (->> (filter #(<= % day) (keys repr-date->fish))
                                 (filter #(= (mod day 7) (mod % 7)))
                                 (map #(get repr-date->fish %))
                                 (reduce +))]
    (if-not (zero? fish-born-in-8-days)
      (assoc repr-date->fish (+ day 9) fish-born-in-8-days)
      repr-date->fish)))

(->> input
     reproduction-date->nfish
     (#(reduce model-new-fish %
               #_(range 0 80) ;; 1st star
               (range 0 256))) ;; 2nd star
     vals
     (reduce +))
