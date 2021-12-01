(ns aoc2020.day20
  (:require [clojure.string :as string]
            [clojure.set :as set]))

(def opposite {:top :bottom
               :bottom :top
               :left :right
               :right :left})

(defn rotate-body [body]
  (reduce (partial map conj)
          (repeat (count (first body)) (list))
          body))

(defn rotate [{:keys [top right bottom left] :as tile}]
  (-> tile
      (assoc :top (reverse left))
      (assoc :right top)
      (assoc :bottom (reverse right))
      (assoc :left bottom)
      (update :body rotate-body)
      (update :rotation (comp #(mod % 4) inc))))


(defn flip-body [body]
  (map reverse body))

(defn parse-tile [tile]
  (let [[title & rows] (string/split-lines tile)
        id (Integer/parseInt (re-find #"\d+" title))]
    [id {:top (apply list (first rows))
         :right (map last rows)
         :bottom (apply list (last rows))
         :left (map first rows)
         :body (map (comp rest butlast) (rest (butlast rows)))
         :rotation 0
         :flips 0}]))

(defn get-rotations [tile]
  (take 4 (iterate rotate tile)))

(defn flip [{:keys [top right bottom left] :as tile}]
  (-> tile ;;vertical flips
      (assoc :top (reverse top))
      (assoc :right left)
      (assoc :bottom (reverse bottom))
      (assoc :left right)
      (update :body flip-body)
      (update :flips (comp #(mod % 2) inc))))

(defn try-new-tile [[jixaw solved new-neighbors missing] [uid utile]]
  (->> (for [transformed (mapcat (juxt identity flip) (get-rotations utile))
             side '(:top :right :bottom :left)
             jj jixaw
             :let [jtile (val jj)
                   jid (key jj)]
             :when (not (contains? solved jid))
             :when (> 4 (count (get jtile :neighbors '())))
             :when (nil? (get-in jtile [:neighbors side]))
             :when (nil? (get-in transformed [:neighbors (opposite side)]))
             :when (= (get jtile side)
                      (get transformed (opposite side)))]
         [jid transformed side uid])
       ((fn [nn]
          (if (or (empty? nn)
                  (not (apply = (map second nn)))) ;; different rotations of the same
            [jixaw solved new-neighbors (cons [uid utile] missing)]
            [jixaw solved (concat new-neighbors nn) missing])))))

(defn put-neighbors [jixaw [id rotated side nid :as neighbor]]
  (cond-> jixaw
    (not (jixaw nid)) (assoc nid rotated)
    true (assoc-in [id :neighbors side] nid)
    true (assoc-in [nid :neighbors (opposite side)] id)))


(defn solve-puzzle [[f & r]]
  (loop [jixaw (apply assoc {} f)
         solved-keys #{}
         unsolved r]
    (let [[_ _ new-neighbors new-unsolved] (reduce try-new-tile
                                                   [jixaw solved-keys [] []]
                                                   unsolved)
          new-j (reduce put-neighbors jixaw new-neighbors)]
      (if (= unsolved new-unsolved)
        jixaw
        (recur new-j (set/union solved-keys (set (keys jixaw))) new-unsolved)))))

(defn create-image [puzzle]
  (let [topleft (first (filter #(= #{:bottom :right}
                                   (set (keys (:neighbors (val %)))))
                               puzzle))
        topline (take-while some? (iterate (comp (partial get puzzle) :right :neighbors) (second topleft)))]
    (loop [image [topline]]
      (if-let [nextline (->> image
                             last
                             (keep (comp (partial get puzzle) :bottom :neighbors))
                             seq)]
        (recur (concat image [nextline]))
        (->> image
             (map (partial map :body))
             (map (partial apply map concat))
             (apply concat))))
    ))

(def monster
  {0 {18 \O}
   1 {0 \O 5 \O 6 \O 11 \O 12 \O 17 \O 18 \O 19 \O}
   2 {1 \O 4 \O 7 \O 10 \O 13 \O 16 \O}})

(defn mark-monster [x y image]
  (if (every? (fn [[kline vline]]
                (every? (fn [[k _]]
                          (let [c (nth (nth image (+ y kline) ) (+ x k))]
                            (or (= \# c)
                                (= \O c))))
                        vline))
              monster)
    (map-indexed (fn [iy line]
           (map-indexed (fn [ix c]
                          (if-let [m (get-in monster [(- iy y) (- ix x)])]
                            m
                            c))
                line))
         image)
    image))

(defn find-monsters [image]
  (let [xrange (range (- (count (first image)) 19))
        yrange (range (- (count image) 3))]
    (reduce (fn [temp-imagey y]
              (reduce (fn [temp-imagex x]
                        (mark-monster x y temp-imagex))
                      temp-imagey
                      xrange))
            image
            yrange)))

(def puzzle (->>
             "resources/2020/day20"
             slurp
             ;; example
             (#(string/split % #"\n\n"))
             (map parse-tile)
             solve-puzzle))

(comment
  (->>
   (reduce (fn [im f]
             (find-monsters (f im)))
           (find-monsters (create-image puzzle))
           (concat (repeat 8 rotate-body)
                   [flip-body]
                   (repeat 5 rotate-body)))
   (map (comp count (partial filter (partial = \#))))
   (apply +))
  (->> ;;part 1
   puzzle
   (filter #(= 2 (count (:neighbors (val %)))))
   keys
   (apply *)))
