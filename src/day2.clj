(ns day2
  (:require [instaparse.core :as insta]
            [clojure.edn :as edn]
            [clojure.string :as string]
            [clojure.java.io :as io]))

(def whitespace
  (insta/parser "whitespace = #'\\s+'"))

(def game-parser
  (insta/parser
     "game = <'Game'> id <':'> round+
      id = #'\\d+'
      round = pick+ <(';' | #'$')>
      pick = count color <','>?
      count = #'\\d+'
      color = ('red' | 'blue' | 'green')"
     :auto-whitespace whitespace))

(defn parse-game-line
  [s]
  (->> (game-parser s)
       (insta/transform
        {:count edn/read-string
         :color keyword
         :pick (fn [count color]
                 {color count})
         :round merge
         :id edn/read-string
         :game (fn [id & entries]
                 {:id id
                  :rounds (vec entries)})})))

(defn possible-game?
  "Returns true if the game line is possible given the limits"
  [limits game]
  (let [{:keys [rounds]} game]
    (->> (for [round rounds
               color [:red :green :blue]]
           (<= (get round color 0)
               (get limits color)))
         (every? true?))))

(def example
  "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green")

(def limits
  {:red 12, :green 13, :blue 14})

(comment
  (->> (io/reader "day-2-input.txt")
       (line-seq)
       (map parse-game-line)
       (filter (fn [game]
                 (possible-game? limits game)))
       
       (map :id)
       (reduce +)))

;; Part 2
(defn minimum-game-vals
  "Returns the minimum set of "
  [game]
  (let [{:keys [rounds]} game]
    (reduce
     (fn [acc round]
       (into acc (filter (fn [[k v]]
                           (> v (get acc k 0))) round)))
     (first rounds)
     (rest rounds))))

(defn power-set
  "Calculates the power set"
  [mins]
  (reduce * (vals mins)))

(comment
  (->>  (io/reader "day-2-input.txt")
        (line-seq)
        (map parse-game-line)
        (map minimum-game-vals)
        (map power-set)
        (reduce +)))
