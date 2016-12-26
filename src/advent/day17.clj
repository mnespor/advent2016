(ns advent.day17
  (:require [digest :as digest]
            [loom.alg-generic :as alg-generic]))

(def input "njfxhljp")
(def open-set #{\b \c \d \e \f})
(def start-node {:steps ""
                 :x 0
                 :y 0})

(defn done? [node]
  (and (= 3 (:x node))
       (= 3 (:y node))))

(defn up [c node]
  (and (contains? open-set c)
       (< 0 (:y node))
       {:steps (str (:steps node) "U")
        :x (:x node)
        :y (dec (:y node))}))

(defn down [c node]
  (and (contains? open-set c)
       (> 3 (:y node))
       {:steps (str (:steps node) "D")
        :x (:x node)
        :y (inc (:y node))}))

(defn left [c node]
  (and (contains? open-set c)
       (< 0 (:x node))
       {:steps (str (:steps node) "L")
        :x (dec (:x node))
        :y (:y node)}))

(defn right [c node]
  (and (contains? open-set c)
       (> 3 (:x node))
       {:steps (str (:steps node) "R")
        :x (inc (:x node))
        :y (:y node)}))

(defn doors [node]
  (concat (x-doors node) (y-doors node)))

(defn successor-fn [node]
  (let [[u d l r] (digest/md5 (str input (:steps node)))]
    (filter identity [(up u node) (down d node) (left l node) (right r node)])))

;; works like loom's generic bf-path, but considers "end"
;; to be any node that satisfies predicate
(defn bf-path-pred [successors start predicate]
  (when-let [preds (some
                    (fn [[_ pm _]] (when (some predicate (keys pm)) pm))
                    (apply alg-generic/bf-traverse successors start (apply concat {:f vector})))]
    (reverse (alg-generic/trace-path preds (first (filter predicate (keys preds)))))))

(defn solve []
  (bf-path-pred successor-fn start-node done?))
