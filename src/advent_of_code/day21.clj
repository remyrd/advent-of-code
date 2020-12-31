(ns advent-of-code.day21
  (:require [clojure.string :as string]
            [clojure.set :as set]))

(def example
  "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
trh fvjkl sbzzf mxmxvkd (contains dairy)
sqjhc fvjkl (contains soy)
sqjhc mxmxvkd sbzzf (contains fish)")

(defn solve [input]
  (let [allergen->ingredients (reduce (fn [temp-map [ingredients allergens]]
                                        (reduce #(update % (keyword %2) conj (set ingredients))
                                                temp-map
                                                allergens))
                                      {}
                                      input)
        [trivial non-trivial] (->> (map (juxt key (comp #(if (= 1 (count %))
                                                        (first %)
                                                        %)
                                                     (partial apply set/intersection)
                                                     val))
                                        allergen->ingredients)
                                   (group-by #(set? (second %)))
                                   ((juxt #(get % false) #(get % true))))
        all-mapped (->> (loop [solved trivial
                               to-solve non-trivial]
                          (let [ [new-solved remaining] (reduce (fn [[mapped remaining] [allergen ingredients]]
                                                                  (let [known-ingredients (set (map second mapped))
                                                                        apply-known (set/difference ingredients known-ingredients)]
                                                                    (if (= 1 (count apply-known))
                                                                      [(cons [allergen (first apply-known)] mapped) remaining]
                                                                      [mapped (cons [allergen apply-known] remaining)])))
                                                                [solved []]
                                                                to-solve)]
                            (if (empty? remaining)
                              new-solved
                              (recur new-solved remaining))))
                        (into {}))
        part1 (->> input
                   (mapcat first)
                   (remove (set/map-invert all-mapped))
                   count)
        part2 (->> all-mapped
                   (sort-by key)
                   (map second)
                   (string/join ","))
        ]
    [part1 part2]
    ))

(comment 
  (->>
   (slurp "resources/day21")
   ;; example
   string/split-lines
   (map (partial re-seq #"\w+"))
   (map (partial partition-by (partial = "contains")))
   (map (partial remove (partial = '("contains"))))
   solve
   ))
