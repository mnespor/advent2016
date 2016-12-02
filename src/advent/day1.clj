(ns advent.day1
  (:require [clojure.string :as string]))

(def input
  (-> (slurp "./resources/day1/input.txt") string/trim (string/split #", " )))

(defn parse-int [s]
   (Integer. (re-find  #"\d+" s )))

;; Day 1: No Time for a Taxicab

(def home {:heading 0 :lat 0 :lon 0})

(defn turn [heading instruction]
  (case instruction
    "R" (mod (+ heading 90) 360)
    "L" (mod (- heading 90) 360)
    heading))

;; takes a position and an instruction and returns the new position
(defn move [position instruction]
  (let [t (subs instruction 0 1)
        d (parse-int instruction)
        {heading :heading lat :lat lon :lon} position
        new-heading (turn heading t)]
    {:heading new-heading
     :lat (case new-heading
            0 (+ lat d)
            180 (- lat d)
            lat)
     :lon (case new-heading
            90 (+ lon d)
            270 (- lon d)
            lon)}))

;; (reduce move home input)

(defn first-duplicate [coll]
  (reduce (fn [seen x]
            (if-let [x (some #{x} seen)]
              (reduced x)
              (conj seen x)))
          []
          coll))

;; returns intermediate points between start position (inclusive) and end position (exclusive)
(defn intermediates [[start end]]
  (cond
    (< (:lat start) (:lat end)) (map (fn [x] {:lat x :lon (:lon start)}) (range (:lat start) (:lat end)))
    (> (:lat start) (:lat end)) (map (fn [x] {:lat x :lon (:lon start)}) (range (:lat start) (:lat end) -1))
    (< (:lon start) (:lon end)) (map (fn [x] {:lat (:lat start) :lon x}) (range (:lon start) (:lon end)))
    (> (:lon start) (:lon end)) (map (fn [x] {:lat (:lat start) :lon x}) (range (:lon start) (:lon end) -1))))

;(first-duplicate (flatten (map intermediates (partition 2 1 (reductions move home input)))))
; probably more idiomatic to do something like (reduce (reductions move home input) intermediates) with a version of intermediates that takes an accumulator
