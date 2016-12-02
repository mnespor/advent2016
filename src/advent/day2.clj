(ns advent.day2
  (:require [clojure.string :as string]))

(def input (slurp "./resources/day2/input.txt"))
(def codes (-> input (string/split #"\n")))

(def home [1 1])

(defn clamp [x lower upper]
  (max (min x upper) lower))

(defn move [[x y] instruction]
  (case instruction
    \U [x (max 0 (dec y))]
    \D [x (min 2 (inc y))]
    \L [(max 0 (dec x)) y]
    \R [(min 2 (inc x)) y]))

(defn clamp-x [x y]
  (case y
    0 2
    1 (clamp x 1 3)
    2 (clamp x 0 4)
    3 (clamp x 1 3)
    4 2))

(defn clamp-y [x y]
  (case x
    0 2
    1 (clamp y 1 3)
    2 (clamp y 0 4)
    3 (clamp y 1 3)
    4 2))

(def home-2 [0 2])

(defn move-2 [[x y] instruction]
  (case instruction
    \U [x (clamp-y x (dec y))]
    \D [x (clamp-y x (inc y))]
    \L [(clamp-x (dec x) y) y]
    \R [(clamp-x (inc x) y) y]))

;(reduce move home (first codes))
                                        ; 2 1
;(reduce move [2 1] (nth codes 1))

                                        ; 1 2 3
                                        ; 4 5 6
                                        ; 7 8 9
; 6 9 6 4 2

;advent.day2> (reduce move-2 home-2 (first codes))
;[3 2]
;advent.day2> (reduce move-2 [3 2] (nth codes 1))
;[3 3]
;advent.day2> (reduce move-2 [3 2] (nth codes 2))
;[2 3]
;advent.day2> (reduce move-2 [3 2] (nth codes 3))
;[1 1]
;advent.day2> (reduce move-2 [3 2] (nth codes 4))
;[2 1]

;8CB23
