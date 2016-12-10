(ns advent.day5
  (:require digest))

(defn pass-char [s]
  (second (re-find #"^00000(..)" s)))

(def xf (comp pass-char digest/md5 (partial str "ugkcyxxp")))

(def characters (->> (range) (pmap xf) (filter some?)))

; (take 8 characters)

;;("43" "2c" "12" "50" "75" "0f" "6e" "37")
;;f2c730e5
