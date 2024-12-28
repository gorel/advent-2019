(ns advent.intcode
  (:require [clojure.string :as str]
            [advent.core :as core])
  (:gen-class))

(defn read-program [filename]
  (->> (str/split (core/read-input filename) #",")
       (map #(Integer/parseInt %))
       (zipmap (range))))

(def opcode 
  {1 +
   2 *})

(def HALT 99)

(defn sort-map [m]
  (map m (sort (keys m))))

(defn exec 
  ([program] (reduce exec program (range)))
  ([program ip] 
   (let [[op lhs rhs out] (nth (partition-all 4 4 (sort-map program)) ip)]
     (if (= op HALT)
       (reduced program)
       (assoc program out ((opcode op) (program lhs) (program rhs)))))))
