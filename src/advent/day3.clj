(ns advent.day3
  (:require [clojure.string :as string]))

(defn parse-int [s]
   (Integer. (re-find  #"\d+" s )))

(def input (slurp "./resources/day3/input.txt"))

(def triangles (->> input string/split-lines (map #(string/split (string/trim %) #"\s+"))))

(def firsts (mapv first triangles))
(def seconds (mapv second triangles))
(def lasts (mapv last triangles))
(def col-triangles (partition 3 (concat firsts seconds lasts)))

(defn triangle? [triangle]
  (let [triangle (sort (map parse-int triangle))]
    (> (+ (first triangle) (second triangle)) (last triangle))))

; (count (filter triangle? triangles))
