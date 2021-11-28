(ns aoc2020.day22
  (:require [clojure.string :as string]))

(defn score [p1 p2]
  (->> (keep seq [p1 p2])
       first
       (map * (reverse (map inc (range (+ (count p1) (count p2))))))
       (apply +)) )

;; part1
(defn play-game [[p1 p2]]
  (loop [player1 p1
         player2 p2]
    (if (or (empty? player1)
            (empty? player2))
      [player1 player2]
      (let [[top1 top2] (map first [player1 player2])]
        (if (< top1 top2)
          ;; p2 wins round
          (recur (rest player1)
                 (concat (rest player2) [top2 top1]))
          ;; p1 wins round
          (recur (concat (rest player1) [top1 top2])
                 (rest player2)))))))

;; part2
(defn recursive-combat [[p1 p2]]
  (loop [player1 p1
         player2 p2
         history #{}]
    (if (contains? history [player1 player2])
      [player1 '()] ;;player1 wins
      (if (or (empty? player1)
              (empty? player2))
        [player1 player2]
        (let [[[top1 deck1] [top2 deck2]] (map (juxt first rest) [player1 player2])]
          (if (and (<= top1 (count deck1))
                   (<= top2 (count deck2)))
            ;; play recursive combat
            (let [[_ p2wins] (recursive-combat [(take top1 deck1)
                                                (take top2 deck2)])]
              (if (seq p2wins)
                (recur (rest player1)
                       (concat (rest player2) [top2 top1])
                       (conj history [player1 player2]))
                (recur (concat (rest player1) [top1 top2])
                       (rest player2)
                       (conj history [player1 player2]))))
            ;; play classic
            (if (< top1 top2)
              ;; p2 wins round
              (recur (rest player1)
                     (concat (rest player2) [top2 top1])
                     (conj history [player1 player2]))
              ;; p1 wins round
              (recur (concat (rest player1) [top1 top2])
                     (rest player2)
                     (conj history [player1 player2])))))))))

(comment
  (->>
   (slurp "resources/day22")
   (#(string/split % #"\n\n"))
   (map string/split-lines)
   (map rest)
   (map (partial map #(Integer/parseInt %)))
   ;; play-game
   recursive-combat
   (apply score)
   ))
