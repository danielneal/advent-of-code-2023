(ns day1
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as string]))

;; *************************************
;; Part one
;; *************************************

(defn line-digits
  "Returns the numerical digits in a string"
  [s]
  (re-seq #"\d" s))

(defn calibration-value
  "Returns the calibration value for a string"
  [s]
  (let [digits (line-digits s)]
    (edn/read-string
     (str (first digits)
          (last digits)))))

(comment
  (->> (io/reader "day-1-input.txt")
       (line-seq)
       (map calibration-value)
       (reduce +)))

;; answer 55834 - correct

;; *************************************
;; Part two
;; *************************************

(def numbers
  "Maps number strings to digits"
  {"one" 1
   "two" 2
   "three" 3
   "four" 4
   "five" 5
   "six" 6
   "seven" 7
   "eight" 8
   "nine" 9})

(defn line-digits-or-numbers
  "Returns the digits or written numbers in a string"
  [s]
  (let [number-pattern (string/join "|" (keys numbers))]
    (re-seq (re-pattern (format "\\d|%s" number-pattern)) s)))

(defn calibration-value2
  "Returns the calibration value for a string"
  [s]
  (let [digits-or-numbers (line-digits-or-numbers s)
        pair [(first digits-or-numbers)
              (last digits-or-numbers)]]
    (->> pair
         (map (fn [digit-or-number]
                (or (get numbers digit-or-number)
                    digit-or-number)))
         (apply str)
         (edn/read-string))))

;; Returns the calibration value for a document
(comment
  (->> (io/reader "day-1-input.txt")
       (line-seq)
       (map calibration-value2)
       (reduce +)))

;;  answer 53254 - incorrect
;; hint (from reddit) sevenine's calibration value is not 77 but is 79
;; these are not in the examples

;; *************************************
;; Part two - attempt two
;; *************************************

(def reversed-numbers
  "Maps reversed number strings to digits"
  (into {}
        (map (fn [[k v]]
               [(string/reverse k) v]))
        numbers))

(defn line-digits-or-numbers-reversed
  "Returns the digits or reversed numbers in a string"
  [s]
  (let [number-pattern (string/join "|" (keys reversed-numbers))]
    (re-seq (re-pattern (format "\\d|%s" number-pattern)) (string/reverse s))))

(defn calibration-value3
  "Returns the calibration value for a string"
  [s]
  (let [pair [(first (line-digits-or-numbers s))
              (first (line-digits-or-numbers-reversed s))]]
    (->> pair
         (map (fn [digit-or-number]
                (or (get numbers digit-or-number)
                    (get reversed-numbers digit-or-number)
                    digit-or-number)))
         (apply str)
         (edn/read-string))))

(comment
  (->> (io/reader "day-1-input.txt")
       (line-seq)
       (map calibration-value3)
       (reduce +)))

;; Answer 54087
