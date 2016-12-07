(ns advent.day7
  (:require [advent.core :as core]
            [clojure.string :as string]
            [clojure.set :as cs]))

(def input (-> "./resources/day7/input.txt" slurp string/split-lines))

(def hypernet-regex #"\[.*?\]")

(defn supernets [s]
  (string/split s hypernet-regex))

(defn hypernets [s]
  (re-seq hypernet-regex s))

;; negative lookahead: don't match aaaa
(defn abba [s]
  (re-find #"(.)(?!\1)(.)\2\1" s))

(defn tls? [ip]
  (and (empty? (filter abba (hypernets ip)))
       (seq (filter abba (supernets ip)))))

;; (count (filter tls? input))

;; positive lookahead: allow overlapping matches
(defn aba [s]
  (map second (re-seq #"(?=((.)(?!\2).\2))" s)))

(defn invert-aba [s]
  (str (second s) (first s) (second s)))

(defn ssl? [ip]
  (let [abas (->> ip
                  supernets
                  (map aba)
                  (apply concat)
                  set)
        inverted-babs (->> ip
                           hypernets
                           (map aba)
                           (apply concat)
                           (map invert-aba)
                           set)]
    (seq (cs/intersection abas inverted-babs))))
