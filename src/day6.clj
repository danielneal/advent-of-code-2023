(ns day6
  (:require [clojure.string :as string]
            [instaparse.core :as insta]
            [clojure.math :as math]
            [clojure.edn :as edn]
            [medley.core :as m]))

(def whitespace
  (insta/parser "whitespace = #'\\s+'"))

(def races-parser
  (insta/parser
     "race = times <'\n'> distances
      times = <'Time:'> number+ 
      distances = <'Distance:'> number+
      number = #'\\d+'"
     :auto-whitespace whitespace))

(def example
 "Time:      7  15   30
  Distance:  9  40  200")

(defn parse-races
  [s]
  (->> (races-parser s)
       (insta/transform {:number edn/read-string
                         :times vector
                         :distances vector
                         :race (fn [times distances]
                                 (map vector times distances))})))

(defn solve-quadratic
  "Charge time is x, in a quadratic x^2 + bx + c  = 0"
  [[race-time best-distance]]
  (let [b (- race-time)
        c best-distance]
    (for [op [+ -]]
      (/ (op (- b) (math/sqrt (- (* b b) (* 4 c)))) 2))))

(defn charge-times
  [races]
  (for [race races
        :let [[s1 s2] (sort (solve-quadratic race))
              [i1 i2] [(math/ceil s1) (math/floor s2)]]]
    [race (cond (and (= i2 s2) (= i1 s1)) (dec (- i2 i1))
                (or (= i2 s2) (= i1 s1)) (- i2 i1)
                :else (inc (- i2 i1)))]))
(comment
  (->> (charge-times (parse-races example))
       (map second)
       (reduce *)) ;; => 288

  (->> (charge-times (parse-races (slurp "day-6-input.txt")))
       (map second)
       (reduce *)))

;; Part 2, parse as single number
(def races-parser2
  (insta/parser
   "race = time <'\n'> distance
      time = <'Time:'> number+ 
      distance = <'Distance:'> number+
      number = #'\\d+'"
   :auto-whitespace whitespace))

(defn parse-races2
  [s]
  (->> (races-parser2 s)
       (insta/transform {:number str
                         :time (comp edn/read-string str)
                         :diqstance (comp edn/read-string str)
                         :race vector})))

(comment
  (-> (parse-races2 (slurp "day-6-input.txt"))
      (vector)
      (charge-times)
      (first)
      (second)) ; => 21039729
)
