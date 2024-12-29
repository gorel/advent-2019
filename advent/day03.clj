(ns advent.day03
  (:require [clojure.string :as str])
  (:require [advent.core :as core])
  (:gen-class))

(def dir 
  {\U [0 -1]
   \D [0 1]
   \L [-1 0]
   \R [1 0]})

(defrecord Point [x y])

(def origin (->Point 0 0))

(defn manhattan [p1 p2]
  (+
   (abs (- (-> p1 :x) (-> p2 :x)))
   (abs (- (-> p1 :y) (-> p2 :y)))))

(defn sort-points [p1 p2]
  (if (= (:x p1) (:x p2))
    (sort-by :y [p1 p2])
    (sort-by :x [p1 p2])))

(defn add [p dx dy] (->Point (+ dx (:x p)) (+ dy (:y p))))

(defrecord Segment [p1 p2])

(defn segment-from-points [p1 p2]
  (let [[p1 p2] (sort-points p1 p2)]
    (->Segment p1 p2)))

(defn direction [segment] 
  (if (= (-> segment :p1 :x) (-> segment :p2 :x))
         :vertical
         :horizontal))

(defn points
  ([instructions]
    (points instructions (->Point 0 0)))
  ([instructions p]
   (if (empty? instructions)
     [p]
     (let [instruction (first instructions)
           d (dir (first instruction))
           dist (Integer/parseInt (apply str (rest instruction)))
           [dx dy] [(* (first d) dist) (* (second d) dist)]
           p2 (add p dx dy)]
       (cons p2 (points (rest instructions) p2))))))

(defn segments [points]
  (map 
    (fn [[p1 p2]] (segment-from-points p1 p2))
    (partition-all 2 1 points)))

(defn between [n lo hi]
  (and n lo hi (>= n lo) (<= n hi)))

(defn intersection [segment1 segment2]
  ;; each segment is either horizontal or vertical)
  (let [dir1 (direction segment1)
        dir2 (direction segment2)]
    (when (not= dir1 dir2)
      (let [vertical (if (= dir1 :vertical) segment1 segment2)
            horizontal (if (= dir1 :horizontal) segment1 segment2)
            x (-> vertical :p1 :x)
            y (-> horizontal :p1 :y)]
        (when (and (between x (-> horizontal :p1 :x) (-> horizontal :p2 :x))
                   (between y (-> vertical :p1 :y) (-> vertical :p2 :y)))
          (->Point x y))))))

(defn crossings [segments1 segments2]
  (filter some?
          (for [segment1 segments1
                segment2 segments2]
            (intersection segment1 segment2))))

(defn closest [crossing-points]
  (apply min-key (partial manhattan origin) crossing-points))

(defn -main [& argv]
  (let [filename (first argv)
        lines (core/read-lines filename)
        segments1 (segments (points (seq (str/split (first lines) #","))))
        segments2 (segments (points (seq (str/split (second lines) #","))))
        closest-intersection (closest (crossings segments1 segments2))
        part1 (manhattan origin closest-intersection)]
    (println "Part 1:" part1)))
