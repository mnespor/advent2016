(ns advent.day15
  (:require [clojure.string :as string]))

(def test-discs [[5 4]
                 [2 1]])

(def discs [[7 0]
            [13 0]
            [3 2]
            [5 2]
            [17 0]
            [19 7]])

(def discs-2 (conj discs [11 0]))

(defn pass? [disc t]
  (let [[size pos] disc]
    (zero? (mod (+ pos t) size))))

(defn all-pass? [ds t]
  (let [ts (range (inc t) (+ (inc t) (count ds)))]
    (every? identity (map pass? ds ts))))

(defn solve [ds]
  (first (filter (partial all-pass? ds) (rest (range)))))
