(ns aoc2021.day11
  (:require [common.util :refer [parse-lines]]))

(def input (->> (parse-lines "resources/2021/day11")
                (mapv (partial mapv #(- (int %) 48)))))

(def mat-size 10)

(defn increase-energy [m [y x]]
  (update-in m [y x] inc))

(defn flash [m [y x]]
  (let [n (get-in m [y x])]
    (if (< 9 n)
      (let [neighbors (for [nx [(dec x) x (inc x)]
                            ny [(dec y) y (inc y)]
                            :when (not= [y x] [ny nx])
                            :when (and (< -1 nx mat-size) (< -1 ny mat-size))]
                        [ny nx])]
        (as-> m $
          (assoc-in $ [y x] -1)
          (reduce (fn [m' n] (update-in m' n #(if (< % 0) % (inc %)))) $ neighbors)
          (reduce flash $ neighbors)))
      m)))

(defn turn-off
  ([m [y x]]
   (update-in m [y x] #(if (= -1 %) 0 %)))
  ([m [y x] counter]
   (update-in m [y x] #(if (= -1 %) (do (swap! counter inc) 0) %))))

(defn walk-grid
  ([m f]
   (reduce (fn [m' y]
             (reduce (fn [m'' x]
                       (f m'' [y x]))
                     m' (range mat-size)))
           m (range mat-size)))
  ([m f counter]
   (reduce (fn [m' y]
             (reduce (fn [m'' x]
                       (f m'' [y x] counter))
                     m' (range mat-size)))
           m (range mat-size))))

(let [flashes (atom 0)
      _ (last (take 101 (iterate #(-> %
                                      (walk-grid increase-energy)
                                      (walk-grid flash)
                                      (walk-grid turn-off flashes))
                                 input)))]
  @flashes) ;; 1st star

(loop [c 0
       m input]
  (if (reduce #(if (not= 0 %2) (reduced false) true)
              false (for [line m v line] v))
    c
    (recur (inc c) (-> m (walk-grid increase-energy)
                       (walk-grid flash)
                       (walk-grid turn-off))))) ;; 2nd star
