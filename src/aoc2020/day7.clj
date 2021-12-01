(ns aoc2020.day7
  (:require [clojure.string :as string]))

(defn keyword-color [[c1 c2]]
  (keyword (str c1 c2)))

(defn build-map [m [color _ inside]]
  (let [outside (keyword-color color)]
    (reduce (fn [m c]
              (assoc-in m [outside (keyword-color (rest c))] ;; invert the map path for the 1st star
                        (Integer/parseInt (first c))))
            m
            (partition 3 inside))))

(defn find-bags [color m]
  (if-let [colors (keys (color m))]
    (reduce #(concat % (find-bags %2 m)) colors colors)
    [color]))

(defn count-bags [color m]
  (if-let [colors (color m)]
    (reduce #(+ % (* (val %2) (count-bags (key %2) m))) 1 colors)
    1))

 (->> "resources/2020/day7"
    slurp
    string/split-lines
    (remove #(string/ends-with? % "contain no other bags."))
    (map #(string/split % #" "))
    (map (partial remove #(string/starts-with? % "bag")))
    (map (partial partition-by (partial = "contain")))
    (reduce build-map {})
    (count-bags :shinygold) ;; use find-bags for the 1st star
    dec)
