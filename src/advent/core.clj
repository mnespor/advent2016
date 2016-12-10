(ns advent.core)

(defn parse-int [s]
  (and s (Integer. (re-find  #"\d+" s ))))

(defn transpose [v]
  (apply mapv vector v))

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))
