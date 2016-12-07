(ns advent.day7
  (:require [advent.core :as core]
            [clojure.string :as string]
            [clojure.set :as cs]))

(def input (-> "./resources/day7/input.txt" slurp string/split-lines))

(def hypernet-regex #"\[.*?\]")

(defn supernet [s]
  (string/split s hypernet-regex))

(defn hypernet [s]
  (re-seq hypernet-regex s))

;; negative lookahead: don't match \1 again
(defn abba [s]
  (re-find #"(.)(?!\1)(.)\2\1" s))

(defn tls? [ip]
  (and (empty? (filter abba (hypernet ip)))
       (seq (filter abba (supernet ip)))))

;; (count (filter tls? input))

;; positive lookahead: allow overlapping matches
(defn aba [s]
  (map second (re-seq #"(?=((.)(?!\2).\2))" s)))

(defn invert-aba [s]
  (str (second s) (first s) (second s)))

(defn ssl? [ip]
  (let [abas (set (apply concat (map aba (supernet ip))))
        inverted-babs (set (map invert-aba (apply concat (map aba (hypernet ip)))))]
    (seq (cs/intersection abas inverted-babs))))
