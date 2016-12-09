(ns advent.day9
  (:require [advent.core :as core]
            [clojure.string :as string]))

(def input (-> "./resources/day9/input.txt" slurp (string/replace #"[\s]" "")))

(def marker-regex #"\(([\d]+)x([\d]+)\)")

;; surely there's a better way
(defn re-find-index [re s]
  (let [match (re-find re s)]
    {:match match
     :index (if (nil? match) -1
                (string/index-of s (first (vec match))))}))

(defn decompress [acc s]
  (if (zero? (count s))
    acc
    (let [match (re-find-index marker-regex s)]
      (cond
        (= -1 (:index match)) (str acc s)
        (zero? (:index match)) (let [marker (first (:match match))
                                     [length times] (map core/parse-int (rest (:match match)))
                                        ; consume the length of the marker itself plus the length indicated by the marker
                                     consumed (+ (count marker) length)]
                                 (recur (apply str acc (repeat times (subs s (count marker) consumed)))
                                        (subs s consumed)))
        :default (recur (str acc (subs s 0 (:index match)))
                        (subs s (:index match)))))))

; (count (decompress "" input))

(defn decompress-2 [acc s]
  (if (zero? (count s))
    acc
    (let [match (re-find-index marker-regex s)]
      (cond
        (= -1 (:index match)) (+ acc (count s))
        ; drop the marker; push the expansion back into the stream
        (zero? (:index match)) (let [marker (first (:match match))
                                     [length times] (map core/parse-int (rest (:match match)))
                                     consumed (+ (count marker) length)
                                     expansion (apply str (repeat times (subs s (count marker) consumed)))]
                                 (recur acc
                                        (str expansion (subs s consumed))))
        :default (recur (+ acc (:index match))
                        (subs s (:index match)))))))

; (decompress-2 0 input)
