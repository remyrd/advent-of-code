(ns advent-of-code.day19
  (:require [clojure.string :as string]))

(def example
  "42: 9 14 | 10 1
9: 14 27 | 1 26
10: 23 14 | 28 1
1: a
11: 42 31
5: 1 14 | 15 1
19: 14 1 | 14 14
12: 24 14 | 19 1
16: 15 1 | 14 14
31: 14 17 | 1 13
6: 14 14 | 1 14
2: 1 24 | 14 4
0: 8 11
13: 14 3 | 1 12
15: 1 | 14
17: 14 2 | 1 7
23: 25 1 | 22 14
28: 16 1
4: 1 1
20: 14 14 | 1 15
3: 5 14 | 16 1
27: 1 6 | 14 18
14: b
21: 14 1 | 1 14
25: 1 1 | 1 14
22: 14 14
8: 42
26: 14 22 | 1 20
18: 15 15
7: 14 5 | 1 21
24: 14 1

abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa
bbabbbbaabaabba
babbbbaabbbbbabbbbbbaabaaabaaa
aaabbbbbbaaaabaababaabababbabaaabbababababaaa
bbbbbbbaaaabbbbaaabbabaaa
bbbababbbbaaaaaaaabbababaaababaabab
ababaaaaaabaaab
ababaaaaabbbaba
baabbaaaabbaaaababbaababb
abbbbabbbbaaaababbbbbbaaaababb
aaaaabbaabaaaaababaa
aaaabbaaaabbaaa
aaaabbaabbaaaaaaabbbabbbaaabbaabaaa
babaaabbbaaabaababbaabababaaab
aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba")


;; when a rule is completely dereffed into lists of literals
;; concatenate all possible literals so upper level lists can
;; deref the current one
(defn all-concats [coll]
  (reduce (fn [amon aval]
            (for [a amon
                  b aval]
              (str a b))) (first coll) (rest coll)))

(defn generate-matches [line-n rules-map]
  (let [line (get rules-map line-n "")]
    (if (every? (partial re-matches #"^[ab]+$") (apply concat line))
      (apply concat line) ;; all possible combinations of "a" / "b" 
      (->> line
           (mapcat (fn [ordered] ;; sets of substrings waiting to be concated
                     (->> ordered
                          (map #(if (list? %) ;; deref all numbers
                                  %
                                  (generate-matches % rules-map)))
                          all-concats)))))))

(defn super-partition [v]
  (->> v
       (partition-by (partial = "|"))
       (remove (partial = '("|")))))

(defn solve [rule-lines lines]
  (let [rules-map (->> (string/split-lines rule-lines)
                       (map (partial re-seq #"[\d\|ab]+"))
                       (map (juxt first (comp super-partition rest)))
                       (into {})
                       )]
    (-> (set (generate-matches "0" rules-map))
        (filter (string/split-lines lines)))))

(comment
  (->>
   ;; "resources/day19"
   ;; slurp
   example
   (#(string/split % #"\n\n"))
   (apply solve)
   ))
