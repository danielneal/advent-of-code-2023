(ns day8
  (:require [clojure.string :as string]
            [instaparse.core :as insta]
            [clojure.edn :as edn]
            [clojure.math :as math]))


(def whitespace
  (insta/parser "whitespace = #'\\s+'"))

(def document-parser
  (insta/parser
     "document = input map
      input = direction+
      direction = #'[LR]'
      map =  map-entry+
      map-entry = node <'='> <'('> node <','> node <')'>
      node = #'[A-Z0-9]{3}'"
     :auto-whitespace whitespace))

(def example
  "RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)")

(defn parse-document
  [s]
  (->> (document-parser s)
       (insta/transform {:node identity
                         :map-entry (fn [node left right]
                                      [node [left right]])
                         :input (fn [& directions]
                                  (vec directions))
                         :direction {"R" 1 "L" 0}
                         :map (fn [& kvs]
                                (into {} kvs))
                         :document (fn [input map]
                                     {:in input
                                      :m map})})))

(defn follow
  [{:keys [in m]}]
  (reduce (fn [node [direction i]]
            (let [next (get-in m [node direction])]
              (if (= node "ZZZ") (reduced i)
                  next)))
          "AAA"
          (map vector (cycle in) (range))))

(comment
  (follow (parse-document (slurp "day-8-input.txt"))))

(def example2
  "LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)")


;; Part 2
(defn z-node?
  [node]
  (string/ends-with? node "Z"))

(defn a-node?
  [node]
  (string/includes? node "A"))

(defn follow2
  "Follow a node until pred returns true"
  [{:keys [in m]} start pred]
  (reduce (fn [[node i :as step] direction]
            (if (pred node)
              (reduced step)
              [(get-in m [node direction])
               (inc i)]))
          [start 0]
          (cycle in)))

(defn z-cycles
  [doc]
  (let [{:keys [m]} doc
        start-nodes (filterv a-node? (keys m))]
    (for [node start-nodes
          :let [[next-z i1] (follow2 doc node z-node?)
                [subsequent-z i2] (follow2 doc node z-node?)]]
      [node i1 next-z i2 subsequent-z])))

;; Exploring the data
(z-cycles (parse-document (slurp "day-8-input.txt")))

;; start steps first steps second
[["VMA" 20093 "MTZ" 20093 "MTZ"]
 ["LBA" 13301 "GPZ" 13301 "GPZ"]
 ["QKA" 12169 "QXZ" 12169 "QXZ"]
 ["JMA" 18961 "VHZ" 18961 "VHZ"]
 ["RKA" 22357 "NDZ" 22357 "NDZ"]
 ["AAA" 20659 "ZZZ" 20659 "ZZZ"]]

;; The Z nodes are in a cycle,
;; therefore we can take the lowest common multiple

;; Use Euclid's algorithm
(defn greatest-common-divisor
  [a b]
  (let [[a b] (sort [a b])
        q (quot b a)
        r (rem b a)]
    (if (pos? r)
      (greatest-common-divisor r a)
      a)))

(defn lowest-common-multiple
  [a b]
  (/ (* a b) (greatest-common-divisor a b)))

(let [numbers (map second (z-cycles (parse-document (slurp "day-8-input.txt"))))]
  (reduce lowest-common-multiple numbers))
;; => 15690466351717
