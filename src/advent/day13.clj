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
  (filter m-open? [[(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)]]))

;; loom counts the starting node
(defn solve [point]
  (dec (count (alg-generic/dijkstra-path successors (constantly 1) [1 1] point))))

(defn all-points [steps] (filter m-open? (combinatorics/selections (range 0 (+ 2 steps)) 2)))

(defn reachable? [steps point]
  (let [distance (solve point)]
    (and (pos? distance)
         (>= steps distance))))

(defn solve-2 [steps]
  (count (filter (partial reachable? steps) (all-points steps))))

;; garbage
(defn vis-open? [[x y] _]
  (if (open? [y x]) " " "#"))
(def visualization (matrix/emap-indexed vis-open? (matrix/matrix (repeat 50 (repeat 50 " ")))))

(def path (alg-generic/dijkstra-path successors (constantly 1) [1 1] [31 39]))
(def path-visualization (matrix/set-indices visualization (map reverse path) (repeat (count path) ".")))
;; (matrix/pm path-visualization)
