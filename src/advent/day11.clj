(ns advent.day11
  (:require [advent.core :as core]
            [clojure.string :as string]
            [clojure.math.combinatorics :as combinatorics]
            [clojure.set :as set]
            [loom.alg-generic :as alg-generic]))

;; what, parsing?
;;The first floor contains a promethium generator and a promethium-compatible microchip.
;;The second floor contains a cobalt generator, a curium generator, a ruthenium generator, and a plutonium generator.
;;The third floor contains a cobalt-compatible microchip, a curium-compatible microchip, a ruthenium-compatible microchip, and a plutonium-compatible microchip.
;;The fourth floor contains nothing relevant.

(def initial-world {:elevator 0
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

(def part-2-world {:elevator 0
                   :floors [#{{:type :gen
                               :id :el}
                              {:type :chip
                               :id :el}
                              {:type :gen
                               :id :di}
                              {:type :chip
                               :id :di}
                              {:type :gen
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

(def goal-world {:elevator 3
                 :floors [#{}
                          #{}
                          #{}
                          #{{:type :gen
                             :id :pm}
                            {:type :chip
                             :id :pm}
                            {:type :gen
                             :id :co}
                            {:type :gen
                             :id :cm}
                            {:type :gen
                             :id :ru}
                            {:type :gen
                             :id :pu}
                            {:type :chip
                             :id :co}
                            {:type :chip
                             :id :cm}
                            {:type :chip
                             :id :ru}
                            {:type :chip
                             :id :pu}}]})

(def part-2-goal {:elevator 3
                  :floors [#{}
                           #{}
                           #{}
                           #{{:type :gen
                              :id :el}
                             {:type :chip
                              :id :el}
                             {:type :gen
                              :id :di}
                             {:type :chip
                              :id :di}
                             {:type :gen
                              :id :pm}
                             {:type :chip
                              :id :pm}
                             {:type :gen
                              :id :co}
                             {:type :gen
                              :id :cm}
                             {:type :gen
                              :id :ru}
                             {:type :gen
                              :id :pu}
                             {:type :chip
                              :id :co}
                             {:type :chip
                              :id :cm}
                             {:type :chip
                              :id :ru}
                             {:type :chip
                              :id :pu}}]})

(defn chip? [item]
  (= (:type item) :chip))

;; it's a p bad idea to take both a chip and a non-matching generator at the same time
(defn bad-idea? [combination]
  (and (< 1 (count combination))
       (let [[x y] combination]
         (and (not= (:type x) (:type y))
              (not= (:id x) (:id y))))))

(defn move [world from to items]
  (-> world
      (assoc :elevator to)
      (update-in [:floors from] set/difference (set items))
      (update-in [:floors to] set/union (set items))))

;; all moves, legal and illegal, except for obviously bad ideas.
;; A move brings one or two items from floor to (inc floor) or to (dec floor)
(defn all-moves [world]
  (let [items (get-in world [:floors (:elevator world)])
        combinations (remove bad-idea? (concat (combinatorics/combinations items 2) (map list items)))
        from (:elevator world)]
    (case from
      0 (map (partial move world from (inc from)) combinations)
      3 (map (partial move world from (dec from)) combinations)
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
  (every? legal-floor? (:floors world)))

(defn legal-moves [world]
  (filter legal-world? (all-moves world)))

(def test-world {:elevator 0
                 :floors [#{{:type :chip
                                :id :h}
                            {:type :chip
                             :id :l}}
                          #{{:type :gen
                             :id :h}}
                          #{{:type :gen
                             :id :l}}
                          #{}]})

(def test-goal-world {:elevator 3
                 :floors [#{}
                          #{}
                          #{}
                          #{{:type :chip
                                :id :h}
                            {:type :chip
                             :id :l}
                            {:type :gen
                             :id :h}
                            {:type :gen
                             :id :l}}]})

;; get a list of legal (non-chip-frying) moves.
;; make each move.

;; loom counts the starting state
(defn solve [world goal]
  (dec (count (alg-generic/bf-path legal-moves world goal))))
