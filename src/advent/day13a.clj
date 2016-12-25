(ns advent.day13a
  (:require [loom.alg-generic :as alg-generic]))

(defn hamming-weight [x]
  (loop [c 0 x x]
    (if (zero? x)
      c
      (recur (inc c) (bit-and x (dec x))))))

(defn open? [fav [x y]]
  (and ((complement neg?) x)
       ((complement neg?) y)
       (even? (hamming-weight (+ fav
                                 (* x x)
                                 (* 3 x)
                                 (* 2 x y)
                                 y
                                 (* y y))))))

(defn successors [fav [x y]]
  (filter (partial open? fav) [[(inc x) y]
                               [(dec x) y]
                               [x (inc y)]
                               [x (dec y)]]))

(defn solve [point fav]
  (alg-generic/dijkstra-path (partial successors fav)
                             (constantly 1)
                             [1 1]
                             point))

(def favorites (filter (partial solve [31 39]) (range)))

(take 20 favorites)
;; (31 523 650 662 664 755 757 761 1172 1344 1350 1352 1358 1362 1364 1792 2167 2171 2173 2370)
