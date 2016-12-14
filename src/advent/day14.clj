(ns advent.day14
  (:require [advent.core :as core]
            [clojure.string :as string]
            digest))

(def input "jlmsuwbz")

(defn stretched-hash [s]
  (nth (iterate digest/md5 s) 2017))

;; this solves part 2; replace stretched-hash with digest/md5 for part 1
(def hashes (map (juxt identity (comp stretched-hash (partial str input))) (range)))

(defn triple-match [[idx hash]]
  [idx (re-find #"(.)\1\1" hash)])

(def triple-matches (->> hashes (map triple-match) (filter (comp some? second))))

(defn next-n [start n coll] (take n (drop start coll)))

(defn key? [[idx [_ c]]]
  (letfn [(has-quint? [[_ hash]]
            (re-find (re-pattern (apply str (repeat 5 c))) hash))]
    (some has-quint? (next-n (inc idx) 1000 hashes))))



;; (take 64 (filter key? triple-matches))
