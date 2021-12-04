(ns aoc2021.day4
  (:require [common.util :refer [parse-lines parse-example]]
            [clojure.string :as str]
            [clojure.set :as set]))

(def example (parse-example "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7"))

(def input (parse-lines "resources/2021/day4"))

(defn vertical-bingos [board]
  (->> board
       (reduce (fn [acc x]
                 (map conj acc x))
               (take 5 (repeat [])))
       (map set)
       set))

(defn diagonal-bingos [board]
  (set
   ((juxt (fn [b]
            (set (map-indexed #(nth %2 %) b)))
          (fn [b]
            (set (map-indexed #(nth %2 (- 4 %)) b)))) board)))

(defn winning-sets [board]
  (set (concat (map set board)
               (vertical-bingos board)
               #_(diagonal-bingos board))))

(def numbers (-> input first (str/split #",")))

(def boards (->> input rest rest
                 (map #(str/split % #" +"))
                 (map #(remove empty? %))
                 (partition 5 6)
                 (map winning-sets)))

(defn won? [ns board]
  (when (some #(= 5 (count (clojure.set/intersection ns %))) board)
    board))

(defn play-bingo-to-win [numbers boards]
  (loop [n (nth numbers 5)
         in-game (take 5 numbers)
         rest-ns (drop 5 numbers)]
    (if-let [winner (some (partial won? (set in-game)) boards)]
      [winner in-game n]
      (recur (first rest-ns)
             (conj in-game (first rest-ns))
             (rest rest-ns)))))

(defn play-bingo-to-lose [numbers boards]
  (loop [n (nth numbers 5)
         bs boards
         in-game (take 5 numbers)
         rest-ns (drop 5 numbers)]
    (if (won? (set in-game) (first bs))
      (if (= 1 (count bs))
        [(first bs) in-game n]
        (recur n
               (let [removed-boards (remove (partial won? (set in-game)) bs)]
                 (if (empty? removed-boards)
                   (list (first bs))
                   removed-boards))
               in-game
               rest-ns))
      (recur (first rest-ns)
             bs
             (conj in-game (first rest-ns))
             (rest rest-ns)))))

(defn sum-unmarked-numbers [[winner-sets in-game-numbers _]]
  (->> winner-sets
       (reduce clojure.set/union)
       (#(clojure.set/difference % (set in-game-numbers)))
       (map read-string)
       (apply +)))

(->> boards
     (play-bingo-to-win numbers)
     ((juxt sum-unmarked-numbers (fn [[_ _ n]] (read-string n))))
     (apply *)) ;; 1st star

(->> boards
     (play-bingo-to-lose numbers)
     ((juxt sum-unmarked-numbers (fn [[_ _ n]] (read-string n))))
     (apply *)) ;; 1st star
;; board of NxN
;; repr as list of numbers 1..N^2

;; (defn win-condition [pos])
