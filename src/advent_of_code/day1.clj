(ns advent-of-code.day1
  (:require [clojure.string :refer [split]]))

(def input (slurp "resources/day1"))

(defn find-sum [[cola colb]]
  (mapcat (fn [a] (keep #(when (= 2020 (+ a %)) [a %]) colb)) cola))

(defn match-last-digit [cola colb]
  (let [mods-a (group-by #(mod % 10) cola)
        mods-b (group-by #(mod % 10) colb)]
    (->> mods-a
         (map (juxt val
                    #(get mods-b
                          (rem (- 10 (key %)) 10))))
         (filter (and first second))
         (map find-sum))))

(defn parsefile [f]
  (->> f
       slurp
       (#(split % #"\n"))
       (map #(Integer/parseInt %))))

(defn solve1 [f]
  (let [nums (parsefile f)
        [odds evens] (vals (group-by even? nums))
        mins #(apply min %)
        bounded (fn [m ns] (filter #(>= (- 2020 m) %) ns))
        spl (fn [col] (->> col
                          (#( bounded (mins %) %))
                          (group-by #(> 1010 %))
                          vals
                          (apply match-last-digit)
                          flatten
                          (#(if (empty? %) 1 (apply * %)))))
        split-evens (spl evens)
        split-odds (spl odds)]
    (* split-evens split-odds)))

(defn butmin [col]
  (let [m1 (apply min col)
        col1 (filter #(not= m1 %) col)
        m2 (apply min col1)
        col2 (filter #(not= m2 %) col1)]
    (conj (remove #(> % (- 2020 (+ m1 m2))) col2)
          m1 m2)))

(defn evens3 [evens]
  (let [sumup (apply + evens)]
    (keep #(when ( = 2020 (- sumup %)) %) evens)))

(defn twooddsoneeven [odds even]
  (->> odds
       (mapcat (fn [o] (keep #(when (not= % o) [% o]) odds)))
       (filter #(< (apply + %) (- 2020 even)))
       distinct))

(defn solve2 [f]
  (let [nums (parsefile f)
        reduce1 (butmin nums)
        [evens odds] (vals (group-by even? reduce1))
        badeven (evens3 evens) ;; nil
        pairedodds (twooddsoneeven odds (apply min evens))]
    (->> (match-last-digit pairedodds evens)
         flatten
         (keep #(when (seq %) (apply * %))))))

(comment (solve1 "resources/day1")
         (solve2 "resources/day1")
         (*  680 857 483))
