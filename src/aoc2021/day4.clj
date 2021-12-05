(ns aoc2021.day4
  (:require [common.util :refer [parse-lines]]
            [clojure.string :as str]
            [clojure.set :as set]))

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
     (apply *)) ;; 2nd star
