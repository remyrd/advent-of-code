(ns aoc2021.day3
  (:require [common.util :refer [parse-lines]]))

(def input (parse-lines "resources/2021/day3" ))


(defn horizontal->vertical [acc line]
  (map str acc line))

(defn gamma-epsilon [[gamma epsilon] freq]
  (let [ones (get freq \1 0)
        zeroes (get freq \0 0)]
    (if (<= zeroes ones)
      [(str gamma \1) (str epsilon \0)]
      [(str gamma \0) (str epsilon \1)])))

(defn bit-rates [in]
  (->> in
       (reduce horizontal->vertical)
       (map frequencies)
       (reduce gamma-epsilon ["" ""])))

(->> input
     bit-rates
     (map #(BigInteger. % 2))
     (apply *)) ;; 1st star

(defn keep-lines [lines bit-rate-fn]
  (loop [coll lines
         n 0]
    (if (= 1 (count coll))
      (first coll)
      (let [model (bit-rate-fn (bit-rates coll))]
        (recur (filterv #(= (nth model n) (nth % n)) coll)
               (inc n))))))
(->> input
     ((juxt #(keep-lines % first)
            #(keep-lines % second)))
     (map #(BigInteger. % 2))
     (apply *)) ;; 2nd star
