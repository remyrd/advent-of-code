(ns aoc2020.day16
  (:require [clojure.string :as string]
            [clojure.set :as set]))

(defn parse-rules [lines]
  (->> lines
       (map #(string/split % #":"))
       (map (juxt first (comp (partial partition 2)
                           (fn [nums] (map #(Integer/parseInt %) nums))
                           (partial re-seq #"\d+")
                           second)))
       (into {})))

(defn parse-ticket [ticket-line]
  (some->> ticket-line
           (re-seq #"\d+")
           (map #(Integer/parseInt %))))

(defn within-bounds [n b]
  (<= (first b) n (second b)))

;; all tickets must have at least one valid field
(defn valid? [rules ticket]
  (let [allrules (mapcat val rules)]
    (every? (fn [n]
              (some (partial within-bounds n) allrules))
            ticket)))

;; start with all possible fields (range ...)
;; filter valid fields for each ticket
(defn rule-positions [tickets [_ bounds]]
  (reduce (fn [valid-fields ticket]
            (filter (fn [field]
                      (some (partial within-bounds (nth ticket field)) bounds))
                    valid-fields))
          (range (count (first tickets)))
          tickets))

(defn solve [[rs mi ti]]
  (let [rules (parse-rules rs)
        mine (last (map parse-ticket mi))
        tickets (->> (keep parse-ticket ti)
                     (filter (partial valid? rules)))]
    (->> rules
         (map (juxt key (partial rule-positions tickets)))
         (sort-by (comp count second))
         (reduce (fn [mapping [rule valid-fields]]
                   (let [already-mapped (set (vals mapping))]
                     (assoc mapping rule (first (set/difference (set valid-fields) already-mapped)))))
                 {})
         (filter #(string/starts-with? (key %) "departure"))
         (map val)
         (map (partial nth mine))
         (apply *)
         )))
  (->>
   "resources/day16"
   slurp
   (#(string/split % #"\n\n"))
   (map string/split-lines)
   solve))
