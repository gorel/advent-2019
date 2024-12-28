(ns advent.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:gen-class))

(defn read-input [filename]
  (->> filename
       io/resource
       slurp
       str/trim))

(defn read-lines [filename]
  (->> filename
       read-input
       str/split-lines))
