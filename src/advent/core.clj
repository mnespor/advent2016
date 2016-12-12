(ns advent.core)

(defn parse-int [s]
  (and s (Integer. (re-find  #"\d+" s ))))

(defn transpose [v]
  (apply mapv vector v))

(defn clamp [x lower-bound upper-bound]
  (max (min x upper-bound) lower-bound))
