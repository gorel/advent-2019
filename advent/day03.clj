(ns advent.day03
  (:require [clojure.string :as str])
  (:require [advent.core :as core])
  (:gen-class))

(def dir 
  {\U [0 1]
   \D [0 -1]
   \L [-1 0]
   \R [1 0]})

(defrecord Point [x y])

(def origin (->Point 0 0))

(defn manhattan [p1 p2]
  (+
   (Math/abs (- (-> p1 :x) (-> p2 :x)))
   (Math/abs (- (-> p1 :y) (-> p2 :y)))))

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

(defn length [segment]
  (manhattan (:p1 segment) (:p2 segment)))

(defn points
  ([instructions]
    (cons origin (points instructions origin)))
  ([instructions p]
   (if (empty? instructions)
     '()
     (let [instruction (first instructions)
           d (dir (first instruction))
           dist (Integer/parseInt (apply str (rest instruction)))
           [dx dy] [(* (first d) dist) (* (second d) dist)]
           p2 (add p dx dy)]
       (cons p2 (points (rest instructions) p2))))))

(defn segments-from [points]
  (map
    (fn [[p1 p2]] (->Segment p1 p2))
    (partition 2 1 points)))

(defn between? [n a b]
  (and (>= n (min a b)) (<= n (max a b))))

(defn intersection [segment1 segment2]
  ;; each segment is either horizontal or vertical)
  (let [dir1 (direction segment1)
        dir2 (direction segment2)]
    (when (not= dir1 dir2)
      (let [vertical (if (= dir1 :vertical) segment1 segment2)
            horizontal (if (= dir1 :horizontal) segment1 segment2)
            [vp1 vp2] (sort-points (:p1 vertical) (:p2 vertical))
            [hp1 hp2] (sort-points (:p1 horizontal) (:p2 horizontal))
            x (:x vp1)
            y (:y hp1)]
        (when (and (between? x (:x hp1) (:x hp2))
                   (between? y (:y vp1) (:y vp2)))
          (->Point x y))))))

(defn dist-to [segments p]
  (let [segment (first segments)
        [p1 p2] [(:p1 segment) (:p2 segment)]
        [x1 y1 x2 y2] [(:x p1) (:y p1) (:x p2) (:y p2)]
        [xp yp] [(:x p) (:y p)]]
    (if (and (between? xp x1 x2)
              (between? yp y1 y2))
      ;; point is on current segment
      (if (= (direction segment) :vertical)
        (Math/abs (- yp y1))
        (Math/abs (- xp x1)))
      ;; point is on a future segment
      (+ (length segment) (dist-to (rest segments) p)))))

(defn intersections [segments1 segments2]
  (->> (for [segment1 segments1
             segment2 segments2]
         (intersection segment1 segment2))
       (filter some?)
       (filter #(not= origin %))
       (map (fn [p] 
              {:point p
               :dist (+ (dist-to segments1 p) (dist-to segments2 p))}))))

(defn closest [intersections]
  (apply min-key #(manhattan origin %) (map :point intersections)))

(defn min-signal [intersections]
  (:dist (apply min-key #(:dist %) intersections)))

(defn -main [& argv]
  (let [filename (first argv)
        lines (core/read-lines filename)
        segments1 (segments-from (points (seq (str/split (first lines) #","))))
        segments2 (segments-from (points (seq (str/split (second lines) #","))))
        intersections (intersections segments1 segments2)
        closest-intersection (closest intersections)
        part1 (manhattan origin closest-intersection)
        part2 (min-signal intersections)]
    (println "Part 1:" part1)
    (println "Part 2:" part2)))
