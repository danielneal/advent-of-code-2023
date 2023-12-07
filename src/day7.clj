(ns day7
  (:require [clojure.string :as string]
            [instaparse.core :as insta]
            [clojure.edn :as edn]))

(def hand-classification-frequencies
  [[:five-of-a-kind [5]]
   [:four-of-a-kind [1 4]]
   [:full-house [2 3]]
   [:three-of-a-kind [1 1 3]]
   [:two-pair [1 2 2]]
   [:one-pair [1 1 1 2]]
   [:high-card [1 1 1 1 1]]])

(def score-hand-class
  (into {} (map (fn [[k] i] [k i]) (reverse hand-order) (range))))

(def score-card
  (into {} (map (fn [c i] [c i]) (reverse "AKQJT98765432") (range))))

(defn classify-hand
  [hand]
  (let [hand-frequencies (sort (vals (frequencies hand)))]
    (->> (for [[k v] hand-classification-frequencies
               :when (= hand-frequencies v)] k)
         (first))))

(def whitespace
  (insta/parser "whitespace = #'\\s+'"))

(def hands-parser
  (insta/parser
     "game = round+
      round = hand bid
      bid = number
      hand = #'[AKQJT2-9]+'
      number = #'\\d+'"
     :auto-whitespace whitespace))

(def example
  "32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483
")

(defn parse-game
  [s]
  (->> (hands-parser s)
       (insta/transform {:number edn/read-string
                         :round (fn [& kvs]
                                  (into {} kvs))
                         :game vector})))

(defn score-hand
  [hand]
  [(score-hand-class (classify-hand hand))
   (mapv score-card hand)])

(comment
  (->> (parse-game example)
       (sort-by (fn [{:keys [hand bid]}] (score-hand hand)))
       (map vector (map inc (range)))
       (map (fn [[rank {:keys [bid]}]]
              (* rank bid)))
       (reduce +)))

;; Part 2 - introduce a wildcard
(defn classify-hand2
  [hand]
  #dbg! (let [hand-frequencies (frequencies hand)
              wildcard-count (get hand-frequencies \J)
              sorted-frequencies (sort (vals (dissoc hand-frequencies \J)))
              best-hand-frequencies (conj (vec (butlast sorted-frequencies))
                                          (+ (or (last sorted-frequencies) 0)
                                             (or wildcard-count 0)))]
          (->> (for [[k v] hand-classification-frequencies
                     :when (= best-hand-frequencies v)] k)
               (first))))

(def score-card2
  (into {} (map (fn [c i] [c i]) (reverse "AKQT98765432J") (range))))

(defn score-hand2
  [hand]
  [(score-hand-class (classify-hand2 hand))
   (mapv score-card2 hand)])

(comment
  (->> (parse-game (slurp "day-7-input.txt"))
       (sort-by (fn [{:keys [hand bid]}] (score-hand2 hand)))
       (map vector (map inc (range)))
       (map (fn [[rank {:keys [bid]}]]
              (* rank bid)))
       (reduce +)))
