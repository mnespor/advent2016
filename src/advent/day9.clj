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


;(time (decompress-2 0 input))
;"Elapsed time: 1.2257759367112E7 msecs"
;11658395076

(defn first-region-length [s]
  (let [match (re-find-index marker-regex s)]
    (cond
      (= -1 (:index match)) (count s)
      (zero? (:index match)) (let [[match length] (:match match)
                                   match (count match)
                                   length (core/parse-int length)]
                               (+ match length))
      :default (:index match))))

;; Split by the outermost marker that covers each boundary.
;; Assume markers never cross boundaries.
(defn split-by-region [acc s]
  (if (zero? (count s)) acc
      (let [length (first-region-length s)]
        (recur (conj acc (subs s 0 length))
               (subs s length)))))

;; this function got a little out of hand.
;; If this region has no markers, split-by-region has ensured that it is not encompassed by any marker. Return its length.
;; Else, find the repeat count (parse-int (last match)).
;; Drop the marker.
;; Split the remaining part of the region (less the marker) and sum the size of each subregion.
;; Multiply that sum by the repeat count and bubble up.

;; region either starts with a marker, or contains no markers at all
(defn expansion-size [region]
  (if-let [match (re-find marker-regex region)]
    (let [times (core/parse-int (last match))
          marker-length (count (first match))
          subregions (split-by-region [] (subs region marker-length))]
      (* times (reduce + (map expansion-size subregions))))
    (count region)))

;; this looks exactly like the recursive part of expansion-size... might be able to
;; eliminate this with a refactor?
(defn decompress-3 [s]
  (reduce + (map expansion-size (split-by-region [] s))))
