(ns advent.day04
  (:require [clojure.string :as str])
  (:require [advent.core :as core])
  (:gen-class))

(defn digits [n]
  (mapv #(Integer/parseInt (str %)) (str n)))

(defn number [digits]
  (reduce #(+ (* 10 %1) %2) 0 digits))

(defn has-double? [digits]
  (some #(>= (count %) 2) (partition-by identity digits)))

(defn has-double-strict? [digits]
  (some #(= (count %) 2) (partition-by identity digits)))

(defn -main [& argv]
  (let [[lo hi] (map
                  Integer/parseInt
                  (str/split (core/read-input (first argv)) #"-"))
        valid (->> (range lo (inc hi))
                   (map digits)
                   (filter #(apply <= %))
                   (filter has-double?))
        valid2 (filter has-double-strict? valid)]
    (println "Part 1:" (count valid))
    (println "Part 2:" (count valid2))))
