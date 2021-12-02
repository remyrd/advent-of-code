(ns common.util
  (:require [clojure.string :as str]))

(defn parse-lines [file]
  (->> file
       slurp
       str/split-lines))
