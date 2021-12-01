(ns aoc2020.day19
  (:require [clojure.string :as string]))

(defn super-partition [v]
  (->> v
       (partition-by (partial = "|"))
       (remove (partial = '("|")))))

(defn coll-to-int [coll]
  (map #(case %
          ("a" "b") (first %)
          (Integer/parseInt %))
       coll))

(defn solve-for-line [remainders rule rules-map]
  (let [current-rule (get rules-map rule)]
    (if (char? (first (first current-rule)))
      (for [remainder remainders
            :let [[c & r] remainder]
            :when (= c (first (first current-rule)))]
        r)
      (for [remainder remainders
            seq-rule current-rule
            r (reduce #(solve-for-line % %2 rules-map) (list remainder) seq-rule) ]
        r))))

(defn solve [rule-lines all-lines]
  (let [rules-map (->> (string/split-lines rule-lines)
                       (map (partial re-seq #"[\d\|ab]+"))
                       (map (juxt (comp #(Integer/parseInt %) first)
                                  (comp (partial map coll-to-int) super-partition rest)))
                       (into {}))
        lines (string/split-lines all-lines)]
    (->> (for [line lines]
           (solve-for-line (list line) 0 rules-map))
         (remove empty?)
         (filter (comp nil? first))
         count)))

(comment
  (->>
   "resources/2020/day19"
   slurp
   (#(string/split % #"\n\n"))
   (apply solve)
   ))
