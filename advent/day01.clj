(ns advent.day01
  (:gen-class))

(defn read-lines []
  (let [reader (java.io.BufferedReader. *in*)]
    (line-seq reader)))

(defn basic [mass]
  (- (quot mass 3) 2))

(defn total [mass]
  (let [fuel (basic mass)]
    (if (<= fuel 0) 0 (+ fuel (total fuel)))))

(defn fuel [fuel-fn masses]
  (transduce (map fuel-fn) + masses))

(defn -main []
  (let [lines (read-lines)
        masses (map Integer/parseInt lines)
        part1 (fuel basic masses)
        part2 (fuel total masses)]
  (println "Part 1:" part1)
  (println "Part 2:" part2)))
