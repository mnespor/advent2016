(ns advent.day4
  (:require [clojure.string :as string]))

(def test-rooms
  ["aaaaa-bbb-z-y-x-123[abxyz]"
   "a-b-c-d-e-f-g-h-987[abcde]"
   "not-a-real-room-404[oarel]"
   "totally-real-room-200[decoy]"])

(defn parse-int [s]
   (Integer. (re-find  #"\d+" s )))

(def input (-> (slurp "./resources/day4/input.txt") string/split-lines))
(def rooms (map room input))

(def room-regex #"^([^\d]+)([\d]+)\[([a-z]+)\]")

(defn checksum [room-name]
  (let [room-name (string/replace room-name "-" "")]
    (->> room-name
         frequencies
         (sort-by val >)
         (partition-by second)
         (map #(sort-by first %))
         (apply concat)
         (map first)
         (take 5)
         (apply str))))


(defn room [s]
  (let [[_ room-name sector room-checksum] (re-find room-regex s)]
    {:name room-name
     :sector (parse-int  sector)
     :checksum room-checksum}))

(defn valid? [r]
  (= (:checksum r) (checksum (:name r))))

(def sector-if-valid (comp (filter valid?) (map :sector)))

(def valid-rooms (filter valid? rooms))

;(transduce sector-if-valid + rooms)

(defn decrypt-letter [sector c]
  (-> c int (- 96) (+ sector) (mod 26) (+ 96) char))

(defn decrypt [r]
  [(->> r :name (map (partial decrypt-letter (:sector r))) (apply str)) (:sector r)])
