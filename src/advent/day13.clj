(ns advent.day13
  (:require [loom.alg-generic :as alg-generic]
            [loom.alg :as alg]))

(def input 1358)
#_(def input 10)
(def target [31 39])

(defn hamming-weight [x]
  (loop [c 0 x x]
    (if (zero? x)
      c
      (recur (inc c) (bit-and x (dec x))))))

(defn open? [[x y]]
  (or (neg? x)
        (neg? y)
        (even? (hamming-weight (+ input (* x x) (* 3 x) (* 2 x y) y (* y y))))))

(defn successors [[x y]]
  (filter open? [[(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)]]))

;; loom counts the starting node
(defn solve []
  (dec (count (alg-generic/dijkstra-path successors (constantly 1) [1 1] [31 39]))))
