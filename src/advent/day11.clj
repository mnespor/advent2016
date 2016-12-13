(ns advent.day11
  (:require [advent.core :as core]
            [clojure.string :as string]
            [clojure.math.combinatorics :as combinatorics]
            [clojure.set :as set]))

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
;; return (min (map recur)).
;; If we encounter a world we've seen before, terminate.
;; If we get stuck on a floor with no legal moves, terminate. maybe with Integer/MAX_VALUE?
;; If everything is on the fourth floor, terminate and return the number of moves taken

(defn chip? [item]
  (= (:type item) :chip))

;; chip is not fried if the floor contains no gens or if it contains


;; it's a p bad idea to take both a chip and a non-matching generator at the same time
(defn bad-idea? [combination]
  (and (< 1 (count combination))
       (let [[x y] combination]
         (and (not= (:type x) (:type y))
              (not= (:id x) (:id y))))))

(defn move [world from to items]
  (-> world
      (update-in [:seen] conj (:floors world))
      (update-in [:moves] inc)
      (assoc :elevator to)
      (update-in [:floors from] set/difference items)
      (update-in [:floors to] set/union items)))

;; all moves, legal and illegal, except for obviously bad ideas.
;; A move brings one or two items from floor to (inc floor) or to (dec floor)
(defn all-moves [world]
  (let [items (get-in world [:floors (:elevator world)])
        combinations (remove bad-idea? (concat (combinatorics/combinations items 2) (map list items)))
        from (:elevator world)]
    (case from
      0 (map (partial move world from (inc from)) combinations)
      3 (map (partial move world (dec from)) combinations)
      (concat (map (partial move world from (inc from)) combinations)
              (map (partial move world from (dec from)) combinations)))))

;; does floor have the corresponding generator for chip?
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
