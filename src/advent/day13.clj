(ns advent.day13
  (:require [loom.alg-generic :as alg-generic]
            [loom.alg :as alg]
            [clojure.core.matrix :as matrix]
            [clojure.math.combinatorics :as combinatorics]))

(def input 1358)
#_(def input 10)
(def target [31 39])

(defn hamming-weight [x]
  (loop [c 0 x x]
    (if (zero? x)
      c
      (recur (inc c) (bit-and x (dec x))))))

(defn open? [[x y]]
  (and ((complement neg?) x)
       ((complement neg?) y)
       (even? (hamming-weight (+ input (* x x) (* 3 x) (* 2 x y) y (* y y))))))

(def m-open? (memoize open?))

(defn successors [[x y]]
  (filter open? [[(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)]]))

;; loom counts the starting node
(defn solve [point]
  (dec (count (alg-generic/dijkstra-path successors (constantly 1) [1 1] point))))

(defn all-points [steps] (combinatorics/selections (range 0 (+ 2 steps)) 2))

(defn reachable? [steps point]
  (let [distance (solve point)]
    (and (pos? distance)
         (>= steps distance))))

;; make sure to count starting point
(defn solve-2 [steps]
  (inc (count (filter (partial reachable? steps) (all-points steps)))))

;; garbage
(defn blah [point]
  (let [meh (solve point)]
    (cond
      (neg? meh) " "
      (> meh 50) "."
      :default (str meh))))
(defn vis-open? [[x y] _]
  (if (open? [y x]) (blah [y x]) "  "))
(def visualization (matrix/emap-indexed vis-open? (matrix/matrix (repeat 27 (repeat 27 " ")))))

#_(def path (alg-generic/dijkstra-path successors (constantly 1) [1 1] [31 39]))
#_(def path-visualization (matrix/set-indices visualization (map reverse path) (repeat (count path) ".")))
;; (matrix/pm path-visualization)
