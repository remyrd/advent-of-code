(ns advent-of-code.day14
  (:require [clojure.string :as string]))

(def mask (->> (slurp "resources/day14")
               string/split-lines
               first
               (#(string/split % #" "))
               last
               reverse))

(defn exp [b e]
  (apply * (repeat e b)))

(def powersof2 (take-while #(not= 0 %) (iterate #(quot % 2) (exp 2 35))))

(defn n->bin [n]
  (first (reduce (fn [[bin nn] pow]
                    (if (> pow nn)
                      [(cons 0 bin) nn]
                      [(cons 1 bin) (- nn pow)]))
                  [[] n] powersof2)))

(defn apply-mask [mask n]
  (let [bin (n->bin n)]
    bin))

(comment (->>
           (slurp "resources/day14")
           string/split-lines
           rest
           (map (partial re-seq #"mem\[(\d+)\] = (\d+)"))
           (map first)
           (map (juxt second #(nth % 2)))
           )
         (apply-mask 8 mask)
         (exp 3 2)
         (re-seq #"mem\[(\d+)\] = (\d+)" "mem[88] = 123"))

