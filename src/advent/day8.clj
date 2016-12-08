(ns advent.day8
  (:require [advent.core :as core]
            [clojure.string :as string]
            [clojure.core.matrix :as matrix]))

(def input (-> "./resources/day8/input.txt" slurp string/split-lines))

(def screen (matrix/zero-matrix 6 50))

(defn rect [m w h]
  (matrix/set-selection m (range 0 h) (range 0 w) 1))

(defn rotate-column [m x by]
  (matrix/set-column m
                     x
                     (rotate-vec (matrix/get-column m x) by)))

(defn rotate-row [m y by]
  (matrix/set-row m
                  y
                  (rotate-vec (matrix/get-row m y) by)))

(defn rotate-vec [v by]
  (as-> v wrap (take-last by wrap) (concat wrap v) (drop-last by wrap)))

(defn command [m s]
  (let [[a b] (->> s (re-seq #"[\d]+") (map core/parse-int))]
    (cond
      (re-find #"rect" s) (rect m a b)
      (re-find #"rotate column" s) (rotate-column m a b)
      (re-find #"rotate row" s) (rotate-row m a b))))

(defn solve []
  (matrix/esum (reduce command screen input)))

; (matrix/pm (reduce command screen input))
