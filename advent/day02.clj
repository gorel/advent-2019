(ns advent.day02
  (:require [clojure.string :as str]
            [advent.intcode :as intcode])
  (:gen-class))

(defn find-noun-verb [prog target]
  (-> (for [noun (range 100)
            verb (range 100)
            :when (= target ((intcode/exec (-> prog
                                               (assoc 1 noun)
                                               (assoc 2 verb))) 0))]
        [noun verb])
      first))

(defn -main [& argv]
  (let [filename (first argv)
        prog (-> (intcode/read-program filename)
                 (assoc 1 12)
                 (assoc 2 2))
        part1 ((intcode/exec prog) 0)
        [noun verb] (find-noun-verb prog 19690720)
        part2 (+ (* noun 100) verb)]
    (println "Part 1:" part1)
    (println "Part 2:" part2)))
