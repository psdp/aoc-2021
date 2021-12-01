(ns aoc.day-1
  (:require [clojure.string :as str]
            [aoc.util :as util]))

(def sample-data "199
200
208
210
200
207
240
269
260
263")

(defn parse-data [data]
  (->> sample-data
      str/split-lines
      (mapv #(Long/parseLong %))))

(def parsed-sample-data (parse-data sample-data))

(def parsed-real-data (util/seq-input-data 1 #(Long/parseLong %)))

;; (defn get-sum [data]
;;  (loop [[x & xs] data
;;         y nil
;;         cnt 0]
;;    (if x
;;      (let [cnt' (if (and y (> x y))
;;                   (inc cnt)
;;                   cnt)]
;;        (recur xs x cnt'))
;;      cnt)))

(defn get-sum [data]
  (->> data
       (map - (rest data))
       (filter pos?)
       count))

(comment
  (get-sum parsed-sample-data);; => 7
  (get-sum parsed-real-data);; => 1448
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; part 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn part-2-sums [data]
  (->> data
       (partition 3 1)
       (map #(apply + %))))

(comment
  (get-sum (part-2-sums parsed-sample-data));; => 5
  (get-sum (part-2-sums parsed-real-data));; => 1471
  )
