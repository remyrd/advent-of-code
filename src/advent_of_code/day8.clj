(ns advent-of-code.day8
  (:require [clojure.string :as string]))

(defn swap-nop-jmp [ins]
  (let [[op n] ins]
    (case op
      "nop" (list "jmp" n)
      "jmp" (list "nop" n)
      nil)))

(defn execute [instructions swap-at]
  (let [target (count instructions)
        swap {swap-at (swap-nop-jmp (nth instructions swap-at))}]
    (loop [i 0
           acc 0
           visited []]
      (if (= i target)
        acc
        (let [[op n] (or (swap i) (nth instructions i))]
          (if (some #(= i %) visited)
            false
            (case op
              "nop" (recur (inc i)
                           acc
                           (cons i visited))
              "acc" (recur (inc i)
                           (+ n acc)
                           (cons i visited))
              "jmp" (recur (+ i n)
                           acc
                           (cons i visited)))))))))

(defn find-swap [instructions]
  (for [run (map-indexed (fn [i ins]
                           (when (swap-nop-jmp ins)
                             (execute instructions i))) instructions)
        :when run]
    run))

(defn parse-line [l]
  (->> l
     (re-seq #"(\w+) ([+-])(\d+)")
     first
     rest
     ((juxt first #(case (second %)
                      "+" (Integer/parseInt (nth % 2))
                      "-" (- 0 (Integer/parseInt (nth % 2))))))))

(comment  (->> "resources/day8"
             slurp
             string/split-lines
             (map parse-line)
             (find-swap)))
