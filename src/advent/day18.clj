(ns advent.day18)

(def input ".^^^.^.^^^.^.......^^.^^^^.^^^^..^^^^^.^.^^^..^^.^.^^..^.^..^^...^.^^.^^^...^^.^.^^^..^^^^.....^....")

(defn tile [triplet]
  (case triplet
    ([\^ \^ \.] [\. \^ \^] [\^ \. \.] [\. \. \^]) \^
    \.))

(defn next-row [row]
  (->> (str \. row \.) (partition 3 1) (map tile) (apply str)))

(defn solve [first-row row-count]
  (->> first-row
       (iterate next-row)
       (take row-count)
       (apply str)
       (filter (partial = \.))
       count))
