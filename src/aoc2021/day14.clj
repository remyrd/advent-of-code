(ns aoc2021.day14
  (:require [clojure.string :as str]))

(defn develop [s rules n counter]
  (if (< counter (quot n 2))
    (let [res (rules s)]
      (str (develop (str (first s) res) rules n (inc counter))
           (apply str (rest
                       (develop (str res (last s)) rules n (inc counter))))))
    s))

(defn init-develop [s m n]
  (println "Memoizing " s)
  (let [res (develop s m n 0)]
    res))

(defn rest-freq [ab s]
  (println "Memoizing freqs for " ab)
  (frequencies (rest s)))

(def mem-develop (memoize init-develop))

(def mem-freqs (memoize rest-freq))

(defn solve [s m n]
  (let [seed-results (map #(mem-develop % m n)
                          (map (partial apply str) (partition 2 1 s)))

        depth-20-str (let [[a & r] seed-results]
                       (str a (apply str (map #(apply str (rest %)) r))))

        get-right-freqs (fn [i ab]
                          (let [s (mem-develop ab m n)
                                freq (mem-freqs ab s)]
                            (if (zero? i)
                              (update freq (first ab) inc)
                              freq)))]
    (->> (map (partial apply str) (partition 2 1 depth-20-str))
         (map-indexed get-right-freqs)
         (apply merge-with +))))

(let [input (slurp "resources/2021/day14")
      [init rules] (str/split input #"\n\n")
      rules (->> (str/split-lines rules)
                 (map #(str/split % #" "))
                 (map (juxt first last))
                 (into {}))]
  (->> (solve init rules 40) ;; 2nd star
       ;; (solve init rules 10) ;; 1st star
       vals
       ((juxt #(apply max %) #(apply min %)))
       (apply -)
       println))
