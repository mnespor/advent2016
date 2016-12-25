(ns advent.day16)

(def test-data [1 0 0 0 0])
(def data   [1 0 0 1 0 0 0 0 0 0 0 1 1 0 0 0 0])
(def data-b [1 1 1 1 0 0 1 1 1 1 1 1 1 0 1 1 0])

(defn invert-bit [x]
  (if (zero? x) 1
      0))

;; one round
(defn dragon [coll]
  (concat coll [0] (->> coll (map invert-bit) reverse)))

(defn dragon-fill-3 [coll length]
  (take length (first (filter #(<= length (count %)) (iterate (memoize dragon) coll)))))

(defn dragon-fill [coll length]
  (if (<= length (count coll))
    (take length coll)
    (recur (dragon coll) length)))

(defn dragon-fill-2 [coll length]
  (take length (first (filter #(<= length (count %)) (iterate dragon coll)))))

(defn pair-checksum [pair]
  (if (reduce = pair)
    1
    0))

(defn checksum [coll]
  (let [c (->> coll (partition 2) (map pair-checksum))]
    (if (odd? (count c))
      c
      (recur c))))

(defn solve [length]
  (apply str (checksum (dragon-fill-2 data length))))

#_(def desired-length 35651584)
;;                    37748718

(def desired-length 272)

(defn iterate-length [length]
  (inc (* 2 length)))

#_(def actual-length 37748735)
(def actual-length 287)

(defn only-middles []
  (nth (iterate dragon []) 21))

(def as-bs-seq (interleave (repeat data) (repeat data-b)))

;; assume a and b sequences alternate so that we don't have to fully reverse
;; the 17Mb sequence
(defn disk-2 [length]
  (take length (flatten (interleave as-bs-seq (only-middles)))))

(defn solve-2 []
  (apply str (checksum disk-2 35651584)))
