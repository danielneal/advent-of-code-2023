(ns day5
  (:require [clojure.edn :as edn]
            [instaparse.core :as insta]
            [medley.core :as m]
            [clojure.math :as math]
            [clojure.string :as string]
            [clojure.java.io :as io]))

(def whitespace
  (insta/parser "whitespace = #'\\s+'"))

(def almanac-parser
  (insta/parser
     "almanac = seeds instructions 
      instructions = map+
      seeds = <'seeds:'> number+ 
      map = map-title map-entry+
      map-title = source-name <'-to-'> destination-name <'map:'>
      source-name = #'[a-z]+'
      destination-name = #'[a-z]+'
      map-entry = destination-start source-start length
      source-start = number
      destination-start = number
      length = number
      number = #'\\d+'"
     :auto-whitespace whitespace))

(defn parse-almanac
  [s]
  (->> (almanac-parser s)
       (insta/transform {:number edn/read-string
                         :source-name keyword
                         :destination-name keyword
                         :seeds (fn [& numbers]
                                  [:seeds (vec numbers)])
                         :map-title (fn [source destination]
                                      [source destination])
                         :map-entry (fn [& kvs]
                                      (into {} kvs))
                         :map (fn [k & vs]
                                {k vs})
                         :instructions (fn [& maps]
                                         [:instructions (apply merge maps)])
                         :almanac (fn [& kvs]
                                    (into {} kvs))})))
(def example
  "seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4")

(defn follow-instructions
  "Given a set of instructions and a source number, return the destination number"
  [instructions number]
  (reduce (fn [number {:keys [destination-start source-start length]}]
            (if (<= source-start number (+ source-start length))
              (reduced (+ number (- destination-start source-start)))
              number))
   number
   instructions))

(defn trace-seeds
  "Traces seeds from seed number to location"
  [almanac]
  (let [{:keys [seeds instructions]} almanac
        maps (m/index-by first (keys instructions))
        history (list [:seed seeds])]
    (loop [[[step ids] :as history] history]
      (let [[_ next-step] (get maps step)
            new-ids (for [id ids]
                      (follow-instructions
                       (get instructions [step next-step])
                       id))]
        (if next-step
          (recur
           (conj history [next-step new-ids]))
          history)))))

(comment
  (->> (slurp "day-5-input.txt")
       (parse-almanac)
       (trace-seeds)
       (into {})
       :location
       (apply min)))

{:some 3
 :nested [{:everyting 3
           :level "nesting"}]}

;; Part 2 - following instructions should work on different datastructures
(defn parse-almanac2
  "Slight modifications to parse-almanac to handle ranges"
  [s]
  (->> (almanac-parser s)
       (insta/transform {:number edn/read-string
                         :source-name keyword
                         :destination-name keyword
                         :seeds (fn [& numbers]
                                  [:seeds (->> numbers
                                               (partition 2)
                                               (map vec)
                                               (sort compare)
                                               (into []))])
                         :map-title (fn [source destination]
                                      [source destination])
                         :map-entry (fn [& kvs]
                                      (into {} kvs))
                         :map (fn [k & instructions]
                                {k (->> (for [{:keys [destination-start source-start length]} instructions]
                                          [[source-start length] destination-start])
                                        (sort compare)
                                        (into []))})
                         :instructions (fn [& maps]
                                         [:instructions (apply merge maps)])
                         :almanac (fn [& kvs]
                                    (into {} kvs))})))

(defn intersection
  "Returns the bounds of the intersection if the two ranges intersect, otherwise nil"
  [[start-a length-a] [start-b length-b]]
  (let [start (max start-a start-b)
        end (min (+ start-a length-a)
                 (+ start-b length-b))
        length (- end start)]
    (when (pos? length)
      [start length])))

(defn compute-intersections
  "Given a set of instructions and a source ranges, creates a map from source range
   to intersecting map ranges, and the offset that should be applied"
  [source instructions]
  (for [[[map-start map-length :as map-range] destination-start] instructions
        :let [intersection (intersection source map-range)]
        :when intersection]
    [intersection (- destination-start map-start)]))

(defn compute-null-ranges
  "Takes a list of intersections and computes the null ranges (pass source range through unchanged)"
  [source intersections]
  (let [[source-start source-length] source]
    (if (empty? intersections)
      ;; No intersections - the whole source range is null
      [[source nil]]
      ;; Otherwise the null ranges are before, between and after the intersection
      (let [[ifirst] (first intersections)
            [ilast] (last intersections)
            source-end (+ source-start source-length)
            ?null-start (when-let [[istart _] (intersection source ifirst)]
                          (let [start source-start
                                length (- istart start)]
                            (when (pos? length)
                              [[[start length] nil]])))
            ?null-end (when-let [[istart ilength] (intersection source ilast)]
                        (let [start (+ istart ilength)
                              length (- source-end start)]
                          (when (pos? length)
                            [[[start (- source-end start)] nil]])))
            ?null-between (->> intersections
                               (partition 2 1)
                               (mapcat (fn [[[[start-a length-a] _]
                                             [[start-b length-b] _]]]
                                         (let [start (+ start-a length-a)
                                               length (- start-b start)]
                                           (when (pos? length)
                                             [[[start length] nil]])))))]
        (concat ?null-start ?null-between ?null-end)))))

(defn trace-seed-ranges
  "Traces seed ranges from source to destination"
  [almanac]
  (let [{:keys [seeds instructions]} almanac
        maps (m/index-by first (keys instructions))
        history (list [:seed (into [] (for [range seeds]
                                        [:init range]))])]
    (loop [[[step ranges] :as history] history]
      (clojure.pprint/pprint history)
      (let [[_ next-step] (get maps step)
            instructions (get instructions [step next-step])
            sources (map second ranges)
            new-ranges (for [[source-start _ :as source] sources
                             :let [intersections (compute-intersections source instructions)
                                   null-ranges (compute-null-ranges source intersections)
                                   all-ranges (concat intersections null-ranges)]
                             [[start length] offset] all-ranges]
                         (if (some? offset)
                           [source [(+ start offset) length]]
                           [source [start length]]))]
        (if next-step
          (recur
           (conj history [next-step new-ranges]))
          history)))))

;; TODO null ranges still exist if there are no intersections
(->> (slurp "day-5-input.txt")
     (parse-almanac2)
     (trace-seed-ranges)
     (into {})
     :location
     (map second)
     (map first)
     (apply min))
