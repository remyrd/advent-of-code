(ns advent-of-code.day20
  (:require [clojure.string :as string]
            [clojure.set :as set]))

(def example "Tile 2311:
..##.#..#.
##..#.....
#...##..#.
####.#...#
##.##.###.
##...#.###
.#.#.#..##
..#....#..
###...#.#.
..###..###

Tile 1951:
#.##...##.
#.####...#
.....#..##
#...######
.##.#....#
.###.#####
###.##.##.
.###....#.
..#.#..#.#
#...##.#..

Tile 1171:
####...##.
#..##.#..#
##.#..#.#.
.###.####.
..###.####
.##....##.
.#...####.
#.##.####.
####..#...
.....##...

Tile 1427:
###.##.#..
.#..#.##..
.#.##.#..#
#.#.#.##.#
....#...##
...##..##.
...#.#####
.#.####.#.
..#..###.#
..##.#..#.

Tile 1489:
##.#.#....
..##...#..
.##..##...
..#...#...
#####...#.
#..#.#.#.#
...#.#.#..
##.#...##.
..##.##.##
###.##.#..

Tile 2473:
#....####.
#..#.##...
#.##..#...
######.#.#
.#...#.#.#
.#########
.###.#..#.
########.#
##...##.#.
..###.#.#.

Tile 2971:
..#.#....#
#...###...
#.#.###...
##.##..#..
.#####..##
.#..####.#
#..#.#..#.
..####.###
..#.#.###.
...#.#.#.#

Tile 2729:
...#.#.#.#
####.#....
..#.#.....
....#..#.#
.##..##.#.
.#.####...
####.#.#..
##.####...
##..#.##..
#.##...##.

Tile 3079:
#.#.#####.
.#..######
..#.......
######....
####.#..#.
.#...#.##.
#.#####.##
..#.###...
..#.......
..#.###...")

(def opposite {:top :bottom
               :bottom :top
               :left :right
               :right :left})

(defn rotate [{:keys [top right bottom left] :as tile}]
  (-> tile
      (assoc :top (reverse left))
      (assoc :right top)
      (assoc :bottom (reverse right))
      (assoc :left bottom)))


(defn parse-tile [tile]
  (let [[title & rows] (string/split-lines tile)
        id (Integer/parseInt (re-find #"\d+" title))]
    [id {:top (apply list (first rows))
         :right (map last rows)
         :bottom (apply list (last rows))
         :left (map first rows)}]))

(defn get-rotations [tile]
  (take 4 (iterate rotate tile)))

(defn get-flips [{:keys [top right bottom left] :as tile}]
  (get-rotations (-> tile ;;vertical flip
                     (assoc :top (reverse top))
                     (assoc :right left)
                     (assoc :bottom (reverse bottom))
                     (assoc :left right))))

(defn try-new-tile [[jixaw solved new-neighbors missing] [uid utile]]
  (->> (for [transformed (concat (get-flips utile) (get-rotations utile))
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

(comment
  (->>
   "resources/day20"
   slurp
   ;; example
   (#(string/split % #"\n\n"))
   (map parse-tile)
   reverse
   solve-puzzle
   (filter #(= 2 (count (:neighbors (val %)))))
   keys
   (apply *)
   )
  (let [tile {:top {:edge '(1 2 3)}
              :right {:edge '(3 6 9)}
              :bottom {:edge '(7 8 9)}
              :left {:edge '(1 4 7)}}]
    (= tile
       (last (take 3 (iterate rotate tile)))))
  )
