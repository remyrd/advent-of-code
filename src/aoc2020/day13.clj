(ns aoc2020.day13
  (:require [clojure.string :as string]))

(defn earliest-bus [[t-s bus-s]]
  (let [buses (->> (string/split bus-s #",")
                 (remove (partial = "x"))
                 (map #(Integer/parseInt %)))
        t (Integer/parseInt t-s)]
    (->> (map #(- % (mod t %)) buses)
       (zipmap buses)
       ((juxt identity #(apply min (vals %))))
       ((fn [[m mi]] (filter #(= mi (val %)) m)))
       first
       (apply *))))

(defn lcm [coll]
  (let [indices (map first coll)
        xs (map second coll)
        maxx (apply max xs)
        maxcoll (first (filter #(= maxx (second %)) coll))
        bar (map #(mod (* -1 %) %2) indices xs)]
    (loop [t (mod (* -1 (first maxcoll)) maxx)]
        (if (empty? (remove zero? (map #(- (mod t %) %2) xs bar) ))
          t
          (recur (+ maxx t))))))


(defn next-pos [[prev-res prev-factor prev-base] [base offset]]
  (let [factor (* prev-base prev-factor)]
    (loop [x 1]
      (let [res (+ prev-res (* factor x))]
        (if (= (mod res base)
               (mod offset base))
          [res factor base]
          (recur (inc x)))))))

(defn solve [coll]
  (let [bar (map (fn [[i x]] [x (mod (* -1 i) x)]) coll)] ;; x_i, -i mod x_i
    (reduce next-pos [0 1 (first (first bar))] (rest bar))))


(comment  (->> "resources/2020/day13"
             slurp
             string/split-lines
             earliest-bus)
          (->>
           (slurp "resources/2020/day13")
           string/split-lines
           second
           (#(string/split % #","))
           (zipmap (range))
           (remove #(= "x" (val %)))
           (map (juxt first #(Integer/parseInt (second %))))
           solve
           first))

