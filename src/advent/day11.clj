(ns advent.day11
  (:require [advent.core :as core]
            [clojure.string :as string]
            [clojure.math.combinatorics :as combinatorics]))

;; what, parsing?
;;The first floor contains a promethium generator and a promethium-compatible microchip.
;;The second floor contains a cobalt generator, a curium generator, a ruthenium generator, and a plutonium generator.
;;The third floor contains a cobalt-compatible microchip, a curium-compatible microchip, a ruthenium-compatible microchip, and a plutonium-compatible microchip.
;;The fourth floor contains nothing relevant.

(def initial-world {:moves 0
                    :seen #{}
                    :elevator 0
                    :floors [#{{:type :gen
                                :id :pm}
                               {:type :chip
                                :id :pm}}
                             #{{:type :gen
                                :id :co}
                               {:type :gen
                                :id :cm}
                               {:type :gen
                                :id :ru}
                               {:type :gen
                                :id :pu}}
                             #{{:type :chip
                                :id :co}
                               {:type :chip
                                :id :cm}
                               {:type :chip
                                :id :ru}
                               {:type :chip
                                :id :pu}}
                             #{}]})

;; get a list of legal (non-chip-frying) moves.
;; make each move.
;; return (min recur).
;; If we encounter a world we've seen before, terminate.
;; If we get stuck on a floor with no legal moves, terminate.
;; If everything is on the fourth floor, terminate and return the number of moves taken

(defn chip? [item]
  (= (:type item) :chip))

;; chip is not fried if the floor contains no gens or if it contains

;; all moves, legal or illegal. A move brings one or two items from floor
;; to inc floor or to dec floor
;; TODO: generate all combinations of two items on the current floor, concat to a list of
;; individual items on the current floor, then try each one both up and down (clamped
;; to floors 0 and 3)
(defn all-moves [world])

(defn has-gen? [floor chip]
  (some #{{:type :gen :id (:id chip)}} floor))

;; legal if the floor if there are no generators
;; or if all chips have corresponding generators
(defn legal-floor? [floor]
  (let [chips (filter chip? floor)]
    (or (= (count chips) (count floor))
        (every? (partial has-gen? floor) chips))))

(defn legal-world? [world]
  (and (not-any? #{(:floors world)} (:seen world))
       (every? legal-floor? (:floors world))))

(defn legal-moves [world]
  (let [{:keys [floors seen elevator]} world]
    ))

(defn win? [world]
  (= 10 (count (get-in world [:floors 3]))))
