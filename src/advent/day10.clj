(ns advent.day10
  (:require [advent.core :as core]
            [clojure.string :as string]
            [loom.graph :as graph]
            [loom.alg :as alg]
            [loom.io :as io]))

(def input (-> "./resources/day10/input.txt" slurp string/split-lines))

(def id-regex #"[\w]+ [\d]+")

;; [source low-dest high-dest]
(defn parse-instruction [s]
  (map parse-value (re-seq #"[\w]+ [\d]+" s)))

;; extracts an int from "value n" but leaves bot and output bin identifiers untouched
(defn parse-value [s]
  (let [[_ match] (re-find #"value ([\d]+)" s)]
    (or (core/parse-int match) s)))

;; takes an instruction. Returns true if the instruction depends on another instruction
(defn child? [instruction]
  (= 3 (count instruction)))

(def all-instructions (map parse-instruction input))
(def children (filter child? all-instructions))

;; does instruction child depend on instruction parent?
(defn is-parent? [child parent]
  (let [known (first child)
        candidates (rest parent)]
    (some #{known} candidates)))

;; given instruction, which parent among instructions satisfies it?
(defn parent-instructions [instructions instruction]
  (filter (partial is-parent? instruction) instructions))

(def deps (graph/digraph (zipmap children (map (partial parent-instructions all-instructions) children))))

(def sorted-instructions (reverse (alg/topsort deps)))

(defn run-instruction [world instruction]
  (if (= 2 (count instruction))
    (let [[chip dest] instruction]
      (update-in world [dest] conj chip))
    (let [[source low-dest high-dest] instruction
          [low-chip high-chip] (sort (get world source))]
      (-> world
          (update-in [low-dest] conj low-chip)
          (update-in [high-dest] conj high-chip)))))

;; (def solution (reduce run-instruction {} sorted-instructions))
;; eyeball-grepped this for "bot 101 (61 17)"

;; part 2
;; (get solution "output 0")
;; (get solution "output 1")
;; (get solution "output 2")
