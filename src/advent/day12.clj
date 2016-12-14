(ns advent.day12
  (:require [clojure.string :as string]
            [advent.core :as core]))

(def initial-state {:pc 0
                    "a" 0
                    "b" 0
                    "c" 0
                    "d" 0})

(defn split-instruction [s]
  (let [parts (string/split s #"[\s]+")]
    [(first parts) (rest parts)]))

(def input (->> "./resources/day12/input.txt"
                slurp
                string/split-lines
                (map split-instruction)
                (into [])))

;; does double duty dereferencing registers and parsing literals
(defn get-reg [state register]
  (or (get state register)
      (core/parse-int register)))

(defn cpy [state x y]
  (-> state (assoc y (get-reg state x)) inc-pc))

(defn inc-pc [state]
  (update-in state [:pc] inc))

(defn increment [state x]
  (-> state (update-in [x] inc) inc-pc))

(defn decrement [state x]
  (-> state (update-in [x] dec) inc-pc))

(defn jnz [state x y]
  (if ((complement zero?) (get-reg state x))
    (update-in state [:pc] + (get-reg state y))
    (inc-pc state)))

(def opcodes {"cpy" cpy
              "inc" increment
              "dec" decrement
              "jnz" jnz})

(def state-2 (assoc initial-state "c" 1))

(defn execute [state]
  (if (<= (count input) (:pc state))
    state
    (recur (let [[opcode args] (get input (:pc state))]
             (apply (get opcodes opcode) state args)))))
