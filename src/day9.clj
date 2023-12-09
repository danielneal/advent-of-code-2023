(ns day9
  (:require [clojure.string :as string]
            [clojure.edn :as edn]))

(defn differences
  "Return the differences of a coll"
  [coll]
  (->> (partition 2 1 coll)
       (map (fn [[a b]] (- b a)))))

(defn next-term
  [coll]
  (let [[d :as ds] (differences coll)]
    (if (apply = ds)
      (+ (last coll) d)
      (+ (last coll) (next-term ds)))))

(defn parse-line
  [s]
  (->> (re-seq #"-?\d+" s)
       (map edn/read-string)))

(defn parse-doc
  [s]
  (->> (string/split s #"\n")
       (map parse-line)))

(comment
  (->> (parse-doc "0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45")
       (map next-term)
       (reduce +)))

;; Part 2
(defn prev-term
  [coll]
  (let [[d :as ds] (differences coll)]
    (if (apply = ds)
      (- (first coll) d)
      (- (first coll) (prev-term ds)))))

(comment
  (->> (parse-doc (slurp "day-9-input.txt"))
       (map prev-term)
       (reduce +)))
