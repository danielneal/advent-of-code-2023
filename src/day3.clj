(ns day3
  (:require [clojure.edn :as edn]
            [instaparse.core :as insta]
            [medley.core :as m]
            [clojure.string :as string]
            [clojure.java.io :as io]))

(def example
  "467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..")

(def line-parser
  (insta/parser
     "<line> = ( number | period | symbol )+
      number = #'\\d+'
      period = '.'
      symbol = #'[^\\d\\.]'"))

(defn parse-input-line
  [s]
  (->> (line-parser s)
       (reduce (fn [acc [tag val]]
                 (case tag
                   :period (conj acc nil)
                   :symbol (conj acc :symbol)
                   :number (into acc
                                 ;; Give every instance of a number
                                 ;; a unique id
                                 (repeat (count val)
                                         {:id (random-uuid)
                                          :val (edn/read-string val)}))))
               [])))

(comment
  (->> (string/split example #"\n")
       (map parse-input-line) 
       (into [])))

(def adjacencies
  (->> (for [dx [-1 0 1]
             dy [-1 0 1]
             :when (not= [dx dy] [0 0])]
         [dx dy])
       (into #{})))

(defn get-total
  [grid]
  (let [symbol-positions (for [[row y] (map vector grid (range))
                               [val x] (map vector row (range))
                               :when (= val :symbol)]
                           [y x])
        parts (for [[y x] symbol-positions
                    [dy dx] adjacencies
                    :let [part (get-in grid [(+ y dy)
                                             (+ x dx)])]
                    :when part]
                part)]
    (->> (m/distinct-by :id parts)
         (map :val)
         (reduce +))))

;; Part 2

(def line-parser2
  (insta/parser
     "<line> = ( number | period | gear | symbol )+
      number = #'\\d+'
      period = '.'
      gear = '*'
      symbol = #'[^\\d*\\.]'"))

(defn parse-input-line2
  [s]
  (->> (line-parser2 s)
       (reduce (fn [acc [tag val]]
                 (case tag
                   (:period :symbol) (conj acc nil)
                   :gear (conj acc :gear)
                   :number (into acc
                                 ;; Give every instance of a number
                                 ;; a unique id
                                 (repeat (count val)
                                         {:id (random-uuid)
                                          :val (edn/read-string val)}))))
               [])))

(defn get-gear-ratios
  [grid]
  (let [possible-gears (for [[row y] (map vector grid (range))
                             [val x] (map vector row (range))
                             :when (= val :gear)]
                         [y x])
        possible-parts (for [[y x] possible-gears
                             [dy dx] adjacencies
                             :let [part (get-in grid [(+ y dy)
                                                      (+ x dx)])]
                             :when (:id part)]
                         (merge part
                                {:pos [y x]}))]
    (->> (partition-by :pos possible-parts)
         (map (fn [parts]
                (let [gears (m/distinct-by :id parts)]
                  (if (= (count gears) 2)
                    (reduce * (map :val gears))
                    0))))
         (reduce +))))

(comment
  (->> (string/split example #"\n")
       (map parse-input-line2) 
       (into [])
       (get-gear-ratios))

  (->> (io/reader "day-3-input.txt")
       (line-seq)
       (map parse-input-line2)
       (into [])
       (get-gear-ratios)))

