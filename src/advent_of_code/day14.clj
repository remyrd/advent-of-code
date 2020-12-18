(ns advent-of-code.day14
  (:require [clojure.string :as string]))

(defn exp [b e]
  (apply * (repeat e b)))

(def powersof2 (take-while #(not= 0 %) (iterate #(quot % 2) (exp 2 35))))

(defn n->bin [n]
  (->> powersof2
       (reduce (fn [[bin nn] pow]
                 (if (> pow nn)
                   [(cons 0 bin) nn]
                   [(cons 1 bin) (- nn pow)]))
               [[] n])
       first
       reverse))

(defn bin->n [bin]
   (apply + (map * powersof2 bin)))

(defn unfloat [addr]
  (loop [addrs [addr]
         i 0]
    (if (< i (count addr))
      (if (= \X (nth (first addrs) i))
        (recur (mapcat #(list (assoc (vec %) i 0)
                              (assoc (vec %) i 1)) addrs)
               (inc i))
        (recur addrs (inc i)))
      addrs)))

(defn apply-mask [mask bin]
  (->> bin
       (map (fn [m b]
              (case m
                \X \X
                \0 b
                \1 1))
            mask)))

;; part 1
;; (defn apply-mask [mask bin]
;;   (->> bin
;;        (map (fn [m b]
;;               (case m
;;                 \X b
;;                 (\0 \1) (Character/digit m 10)))
;;             mask)))

(defn write-mem [mem mask [_ i n]]
  (let [addrs (->> (Integer/parseInt i)
                   n->bin
                   (apply-mask mask)
                   unfloat
                   (map bin->n))]
    (reduce #(assoc % %2 n) mem addrs)))

;; part 1
;; (defn write-mem [mem mask [_ i n]]
;;   (assoc mem i (->> (Integer/parseInt n)
;;                     n->bin
;;                     (apply-mask mask)
;;                     bin->n)))

(defn process-line [{:keys [mask mem] :as res} line]
  (let [seqline (re-seq #"\w+" line)]
    (case (first seqline)
      "mem" (assoc res :mem (write-mem mem mask seqline))
      "mask" (assoc res :mask (second seqline)))))

(comment
  (bin->n (n->bin 42))
  (unfloat (list  0 0 \X 1))
  ;; part 1
  (->>
   (slurp "resources/day14")
   string/split-lines
   (reduce process-line {:mask nil :mem nil})
   (#(get % :mem))
   vals
   (map #(Integer/parseInt %))
   (apply +)))
