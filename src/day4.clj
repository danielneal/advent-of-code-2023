(ns day4
  (:require [clojure.edn :as edn]
            [instaparse.core :as insta]
            [medley.core :as m]
            [clojure.math :as math]
            [clojure.string :as string]
            [clojure.java.io :as io]))

(def whitespace
  (insta/parser "whitespace = #'\\s+'"))

(def card-parser
  (insta/parser
     "card = <'Card'> id <':'> winning-numbers <'|'> chosen-numbers
      id = number
      winning-numbers = number+
      chosen-numbers = number+
      number = #'\\d+'"
     :auto-whitespace whitespace))

(defn parse-card
  [s]
  (->> (card-parser s)
       (insta/transform
        {:number edn/read-string
         :chosen-numbers (fn [& numbers]
                           {:chosen-numbers (set numbers)})
         :winning-numbers (fn [& numbers]
                            {:winning-numbers (vec numbers)})
         :id (fn [id]
               {:id id})
         :card (fn [& kvs]
                 (into {} kvs))})))

(defn count-matching-numbers
  [card]
  (let [{:keys [chosen-numbers winning-numbers]} card]
    (count (filter chosen-numbers winning-numbers))))

(defn score
  [card]
  (int (math/pow 2 (dec (count-matching-numbers card)))))

(def example
  "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11")

(comment
  (->> (string/split example #"\n")
       (map parse-card)
       (map score)
       (reduce +))

  (->> (io/reader "day-4-input.txt")
       (line-seq)
       (map parse-card)
       (map score)
       (reduce +)))

;; part 2

(defn accumulate-cards
  [cards]
  (loop [card-id 1
         cards (->> cards
                    (map (fn [card] (assoc card :copies 1)))
                    (m/index-by :id))]
    (let [{:keys [copies] :as current-card} (get cards card-id)]
      (if (nil? current-card)
        cards
        (let [match-count (count-matching-numbers current-card)]
          (recur
           (inc card-id)
           (reduce (fn [cards id]
                     (update-in cards [id :copies] + copies))
                   cards
                   (range (inc card-id)
                          (inc (+ card-id match-count))))))))))
(comment
  (->> (string/split example #"\n")
       (map parse-card)
       accumulate-cards
       (vals)
       (map :copies)
       (reduce +)) ;;= > 30

  (->> (io/reader "day-4-input.txt")
       (line-seq)
       (map parse-card)
       accumulate-cards
       (vals)
       (map :copies)
       (reduce +)))


