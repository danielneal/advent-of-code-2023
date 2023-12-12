(ns day10
  (:require [clojure.string :as string]
            [instaparse.core :as insta]
            [clojure.math :as math]
            [clojure.edn :as edn]
            [malli.core :as malli]
            [clojure.set :as set]))


(def example
  "..F7.
.FJ|.
SJ.L7
|F--J
LJ...")

(def grid-parser
  (insta/parser
   "grid = (row <#'\n|$'>)+
    row = square+
    square = ground | start | pipe 
    ground = <'.'>
    start = <'S'>
    pipe = ew | ns | nw | ne | sw | se
    ew = <'-'>
    ns = <'|'>
    ne = <'L'>
    nw = <'J'>
    sw = <'7'>
    se = <'F'>"))

(def cardinal-directions {\n [-1 0]
                          \s [1 0]
                          \e [0 1]
                          \w [0 -1]})

(defn parse-grid
  "Parse the grid into a convenient format"
  [s]
  (->> (grid-parser s)
       (insta/transform {:square identity
                         :pipe (fn [[connection]]
                                 (let [[from to] (name connection)]
                                   (mapv cardinal-directions [from to])))
                         :start (constantly :start)
                         :ground (constantly nil)
                         :row vector
                         :grid vector})))

(defn transform-grid
  "Runs f on every [pos square] tuple in grid, returning the grid."
  [grid f]
  (mapv (fn [row y]
          (mapv (fn [square x]
                  (f [y x] square))
                row (range)))
        grid
        (range)))

(defn absolute-square
  "Replace a square of deltas with the positions of entrance and exit"
  [pos square]
  (let [[y x] pos
        [[from-dy from-dx]
         [to-dy to-dx]] square
        entrance [(+ y from-dy)
                  (+ x from-dx)]
        exit [(+ y to-dy)
              (+ x to-dx)]]
    [entrance exit]))

(defn absolute-grid
  "Make the grid absolute by replacing each square with 
   a pair of the positions [entrance exit] it connects."
  [grid]
  (transform-grid
   grid
   (fn [[y x :as pos] square]
     (if (or (nil? square)
             (keyword? square))
       square
       (absolute-square pos square)))))

(defn absolute-start
  "Replaces the start of the grid with a pair [entrance exit] of the positions it connects.
   Returns the grid and the start position"
  [grid]
  (let [start (atom nil)]
    [(transform-grid
      grid
      (fn [[y x :as pos] square]
        (if (= square :start)
          (do (reset! start pos)
              ;; The pipe is correctly oriented when the squares it connects to, connect to it
              (first (for [[from to] ["ew" "ns" "nw" "ne" "sw" "se"]
                           :let [relative-square (mapv cardinal-directions [from to])
                                 [entrance exit :as absolute-square] (absolute-square pos relative-square)]
                           :when (every? (fn [connected]
                                           (when-let [[entrance2 exit2] (get-in grid connected)]
                                             (contains? #{entrance2 exit2} pos)))
                                         [entrance exit])]
                       absolute-square)))
          square)))
     @start]))

(defn step
  "For traversing a pipe.  
   Returns the next square given the grid, current and previous squares"
  [grid prev current]
  (let [[entrance exit] (get-in grid current)]
    (cond
      (= prev entrance) exit
      (= prev exit) entrance)))

(defn traverse
  "Starting at the start, pick a direction, and traverse the pipe, 
   recording every position visited and the distance travelled"
  [[grid start]]
  (loop [d 1
         [prev current] [start (first (get-in grid start))]
         out [[start 0]]]
    (let [next (step grid prev current)
          out (conj out [current d])]
      (if (= next start)
        out
        (recur (inc d)
               [current next]
               out)))))

(comment
  (-> (parse-grid (slurp "day-10-input.txt"))
      (absolute-grid)
      (absolute-start)
      (traverse)
      (last)
      (second)
      (/ 2)
      (math/ceil)))

;; Part 2

(def example2
  "...........
.S-------7.
.|F-----7|.
.||..FF.||.
.||.....||.
.|L-7.F-J|.
.|..|.|..|.
.L--J.L--J.
...........")

;; Need to find the crossings
(defn parse-deltas
  "Parse a sequence of y deltas and find crossing / turning points 
   and countable area"
  [row]
  (malli/parse
   [:*
    [:altn
     [:area [:* nil?]]
     [:turn
      [:alt
       [:cat [:= 1] [:* zero?] [:= 1]]
       [:cat [:= -1] [:* zero?] [:= -1]]]]
     [:cross
      [:alt
       [:enum -2 2]
       [:cat [:= 1] [:* zero?] [:= -1]]
       [:cat [:= -1] [:* zero?] [:= 1]]]]]]
   row))

(defn find-enclosed-space
  "Finds the enclosed space by transforming the grid into the 
   vertical deltas, then scanning the rows looking for crossing 
   points. 
   Each row is parsed to determine crossing vs turning points. 
   If at a turning point, stay inside/outside, if at a crossing point, 
   flip from outside to inside."
  [grid]
  (let [[grid start] (->> (parse-grid grid)
                          (absolute-grid)
                          (absolute-start))
        boundary (->> (traverse [grid start])
                      (into {}))
        delta-grid (transform-grid
                    grid
                    (fn [pos square]
                      (when-let [[[y1 x1] [y2 x2]] square]
                        (when (contains? boundary pos)
                          (- y1 y2)))))]
      (reduce
       (fn [[inside? n :as acc] [tag args]]
         (cond
           (and (= tag :area)
                inside?) [inside? (+ n (count args))]
           (= tag :cross) [(not inside?) n]
           :else acc))
       [false 0]
       (for [deltas delta-grid
             :let [sections (parse-deltas deltas)]
             section sections]
         section))))
