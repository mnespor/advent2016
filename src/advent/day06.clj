(ns advent.day6
  (:require [advent.core :as core]
            [clojure.string :as string]))

(def input (-> "./resources/day6/input.txt" slurp string/split-lines))

(defn solve [v]
  (->> v
       core/transpose
       (map frequencies)
       (map (partial sort-by val >))
       (map ffirst)
       (apply str)))

(defn solve-2 [v]
  (->> v
       core/transpose
       (map frequencies)
       (map (partial sort-by val))
       (map ffirst)
       (apply str)))
