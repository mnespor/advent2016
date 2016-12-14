(ns advent.day13
  (:require [loom.alg-generic :as alg-generic]))

(def input 1358)
(def target [31 39])

(defn hamming-weight [x]
  (loop [c 0 x x]
    (if (zero? x)
      c
      (recur (inc c) (bit-and x (dec x))))))

(def hamming-weights (into [] (map hamming-weight (range 100000))))

(defn wall? [point]
  (let [[x y] point]
    (odd? (hamming-weights (+ input (* x x) (* 3 x) (* 2 x y) y (* y y))))))

(defn successors [point]
  )
