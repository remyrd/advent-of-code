(ns advent-of-code.day15)

(loop [nums->turns {0 1
                    14 2
                    1 3
                    3 4
                    7 5}
       previous-turn 6
       last-spoken 9]
  (if (= 30000000 previous-turn)
    last-spoken
    (let [spoken (- previous-turn (get nums->turns last-spoken previous-turn))]
      (recur (assoc nums->turns last-spoken previous-turn)
             (inc previous-turn)
             spoken))))
