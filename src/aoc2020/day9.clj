(ns aoc2020.day9
  (:require [clojure.string :as string]))

(defn find-sum [preamble target]
  (let [numbers (set preamble)]
    (for [a numbers
          :let [b (- target a)]
          :when (< a b)
          :when (contains? numbers b)]
      target)))


(defn weak-numbers [nums]
  (for [a (range (- (count nums) 26))
        :let [calc-range (take 26 (drop a nums))
              preamble (take 25 calc-range)
              target (last calc-range)]
        :when (empty? (find-sum preamble target))]
    target))

(defn find-subset [numbers target]
  (for [a (range (count numbers))
        :let [b (- (count numbers) a)
              subset (take b numbers)
              total (apply + subset)]
        :when (= target total)
        :while (>= total target)]
    (+ (apply min subset) (apply max subset))))

(defn weak-encryption [numbers target]
  (for [a (range (dec (count numbers)))
        :let [b (find-subset (drop a numbers) target)]]
    b))

(comment  (->> "resources/day9"
             slurp
             string/split-lines
             (map #(Long/parseLong %))
             ((juxt identity (comp first #(weak-numbers %))))
             (apply weak-encryption))
             )


