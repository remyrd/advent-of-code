(ns aoc2020.day23)

(def input [4 7 6 1 3 8 2 5 9])

(defn dec-with-mod [m n]
  (let [nn (mod (dec n) m)]
    (if (zero? nn)
      m
      nn)))

(defn find-destination [removed m n]
  (let [candidates (take 4 (iterate (partial dec-with-mod m) (dec-with-mod m n)))]
    (first (remove (partial contains? (set removed)) candidates))))

(defn tomap [coll]
  (->  (into {} (map vec (partition 2 1 coll)))
       (assoc (last coll) (first coll))))

(defn move2 [size [cup m]]
  (let [removed (take 3 (iterate (partial  m) (m cup)))
        destination (find-destination removed size cup)
        after-dest (m destination)
        after-removed (m (last removed))
        new-map (-> m
                    (assoc cup after-removed)
                    (assoc (last removed) after-dest)
                    (assoc destination (first removed)))]
    [(new-map cup) new-map]
    ))

(defn play2 [turns coll]
  (let [mapped (tomap coll)
        size (count coll)
        [_ cups] (last  (take turns (iterate (partial move2 size) (move2 size [(first coll) mapped]))))]
    (rest (take 9 (iterate (partial cups) 1)))))

;; part 1
(comment
  (->> (play2 100 input)
      (apply str)))

;; part 2
(comment
  (->> 
   (play2 10000000 (concat input (range 10 1000001)))
   (take 2)
   (apply *)))
