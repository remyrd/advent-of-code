(ns aoc2021.day8
  (:require [common.util :refer [parse-lines]]
            [clojure.string :as str]
            [clojure.set :as s]))

(def input (->> (parse-lines "resources/2021/day8")
                #_example
                (map #(str/split % #"\|"))
                (map (fn [strings] (map #(->> (re-seq #"\w+" %)
                                             (map set)) strings)))))

(defmulti dispatch-on-count (fn [_ unk] (count unk)))

(defmethod dispatch-on-count 5 [m unk]
  (let [one (get m 1)
        two (get m 2)
        seven (get m 7)
        three (get m 3)
        four (get m 4)
        five (get m 5)]
    (cond
      (= unk two) m
      (= unk three) m
      (= unk five) m
      (and two five (not= unk two)
           (not= unk five)) (assoc m 3 unk)
      (and three five (not= unk three)
           (not= unk five)) (assoc m 2 unk)
      (and three two (not= unk three)
           (not= unk two)) (assoc m 5 unk)
      (and one (= (s/intersection one unk) one)) (assoc m 3 unk)
      (and seven (= (s/intersection seven unk) seven)) (assoc m 3 unk)
      (= 2 (count (s/intersection unk four))) (assoc m 2 unk)
      (= 3 (count (s/intersection unk five))) (assoc m 2 unk)
      (= 3 (count (s/intersection unk two))) (assoc m 5 unk)
      :else m)))

(defmethod dispatch-on-count 6 [m unk]
  (let [one (get m 1)
        seven (get m 7)
        three (get m 3)
        six (get m 6)
        four (get m 4)
        nine (get m 9)
        zero (get m 0)]
    (cond
      (= unk zero) m
      (= unk nine) m
      (= unk six) m
      (and nine six (not= nine unk)
           (not= six unk)) (assoc m 0 unk)
      (and nine zero (not= nine unk)
           (not= zero unk)) (assoc m 6 unk)
      (and six zero (not= six unk)
           (not= zero unk)) (assoc m 9 unk)
      (= 1 (count (s/intersection one unk))) (assoc m 6 unk)
      (= 2 (count (s/intersection seven unk))) (assoc m 6 unk)
      (and three (= three (s/intersection three unk))) (assoc m 9 unk)
      (= (s/intersection unk four) four) (assoc m 9 unk)
      :else m)))

(defmethod dispatch-on-count :default [m _] m)

(defn solve-numbers [[a b]]
  (let [conc (concat a b)
        m (reduce #(case (count %2)
                     2 (assoc % 1 %2)
                     3 (assoc % 7 %2)
                     4 (assoc % 4 %2)
                     7 (assoc % 8 %2)
                     %)
                  {} conc)]
    (loop [m' m]
      (if (= (set b) (s/intersection (set b) (set (vals m'))))
        (let [inverse-m (into {} (map (fn [[k v]] [v k]) m'))]
          (map inverse-m b))
        (let [result (reduce #(dispatch-on-count % %2) m' conc)]
          (recur result))))))

(->> input
     (map second)
     flatten
     (filter #(#{2 3 4 7} (count %)))
     count) ;; 1st star

(->> input
     (map solve-numbers)
     (map (partial apply str))
     (map #(Integer/parseInt %))
     (apply +)) ;; 2nd star
