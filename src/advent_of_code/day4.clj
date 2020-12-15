(ns advent-of-code.day4
  (:require [clojure.string :as string]
            [clojure.set :as set]))

(def required #{"byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"})

(defmulti validate-kv (partial key))

(defmethod validate-kv "byr" [[_ v]]
  (and (re-find #"^\d{4}$" v)
       (>= 2002 (Integer/parseInt v) 1920)))

(defmethod validate-kv "iyr" [[_ v]]
  (and (re-find #"^\d{4}$" v)
       (>= 2020 (Integer/parseInt v) 2010)))

(defmethod validate-kv "eyr" [[_ v]]
  (and (re-find #"^\d{4}$" v)
       (>= 2030 (Integer/parseInt v) 2020)))

(defmethod validate-kv "hgt" [[_ v]]
  (when-let [[_ n u] (first (re-seq #"^(\d+)(in|cm)$" v))]
    (case u
      "cm" (>= 193 (Integer/parseInt n) 150)
      "in" (>= 76 (Integer/parseInt n) 59)
      false)))

(defmethod validate-kv "hcl" [[_ v]]
  (re-matches #"^\#[0-9a-f]{6}$" v))

(defmethod validate-kv "ecl" [[_ v]]
  (re-matches #"^amb|blu|brn|gry|grn|hzl|oth$" v))

(defmethod validate-kv "pid" [[_ v]]
  (re-matches #"^\d{9}$" v))

(defmethod validate-kv "cid" [_]
  true)

(defmethod validate-kv :default [_]
  true)

(defn valid? [doc]
  (when (not= "" (first doc))
    (let [fields (->> doc
                    (mapcat #(string/split % #" "))
                    (map #(string/split % #":"))
                    (into {}))
          ks (set (keys fields))]
      (and (= required (set/intersection ks required))
           (->> fields
              (filter validate-kv)
              keys
              count
              (= (count ks)))))))

(defn day4 [filename]
  (->> filename
     slurp
     string/split-lines
     (partition-by (partial = ""))
     (filter valid?)
     count))


(comment (day4 "resources/day4"))
