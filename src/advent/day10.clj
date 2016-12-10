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
  (re-seq #"[\w]+ [\d]+" s))

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
