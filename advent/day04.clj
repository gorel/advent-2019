(ns advent.day04
  (:require [clojure.string :as str])
  (:require [advent.core :as core])
  (:gen-class))

(defn has-double? 
  ([digits] (has-double? digits false))
  ([digits strict]
   (let [f (if strict = >=)]
     (some #(f (count %) 2) (partition-by identity digits)))))

(defn -main [& argv]
  (let [[lo hi] (map
                  Integer/parseInt
                  (str/split (core/read-input (first argv)) #"-"))
        valid (->> (range lo (inc hi))
                   (map #(mapv (fn [ch] (Integer/parseInt (str ch))) (str %)))
                   (filter #(apply <= %))
                   (filter has-double?))
        valid2 (filter #(has-double? % true) valid)]
    (println "Part 1:" (count valid))
    (println "Part 2:" (count valid2))))
