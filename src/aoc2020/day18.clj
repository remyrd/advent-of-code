(ns aoc2020.day18
  (:require [clojure.string :as string]))


(def example "5 + (8 * 3 + 9 + 3 * 4 * 3)")

;; isolate priority operations
(defn isolate-priority [coll]
  (let [partitioned (partition-by (partial = '(quote *)) coll)]
    (if (= coll (first partitioned))
      coll
      (reduce (fn [res part]
                (if (= 1 (count part))
                  (concat res part)
                  (concat res (list part))))
              []
              partitioned))))

;; only deal with expr without parens
(defn resolve-internal [coll]
  (loop [c (isolate-priority coll)] ;;2nd star. use [c coll] instead for 1st star
    (let [[a x b & r] c
          resolved-a (if (seq? a)
                       (resolve-internal a)
                       a)
          resolved-b (if (seq? b)
                       (resolve-internal b)
                       b)
          resolved-x (resolve (eval x))
          res (resolved-x resolved-a resolved-b)]
      (if (empty? r)
        res
        (recur (cons res r))))))

(defn solve-line [line]
  (->> line
       (concat "'(")
       (#(concat % [\)]))
       (map #(case %
               (\+ \*) (str "'" %)
               %))
       (apply str)
       read-string
       ;; line is evaled and parenthesis are semantically accurate
       ;; embedded parens translate to embedded lists
       ;; no need to explicitly parse any number B^)
       eval
       resolve-internal
       ))


(comment
  (->>
   "resources/2020/day18"
   slurp
   string/split-lines
   (map solve-line)
   (apply +)))
