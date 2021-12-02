(ns aoc.day-2
  (:require [clojure.string :as str]
            [aoc.util :as util]))

(def sample-data "forward 5
down 5
forward 8
up 3
down 8
forward 2")

(defn parse-line [line]
  (let [[d u] (str/split line #"\s")]
    [(keyword d) (Long/parseLong u)]))

(def parsed-sample-data
  (->> sample-data
       str/split-lines
       (map parse-line)))

(def parsed-real-data
  (util/seq-input-data 2 parse-line))

(defn get-result [parsed-data]
  (apply * (reduce (fn [[x y] [d u]]
                     (case d
                       :forward [(+ x u) y]
                       :down [x (+ y u)]
                       :up [x (- y u)]
                       [x y]))
                   [0 0]
                   parsed-data)))

;;; part 1
(comment
  (get-result parsed-sample-data)
  ;; => 150
  (get-result parsed-real-data)
  ;; => 2147104
  )

(defn get-result-2 [parsed-data]
  (->> (reduce (fn [[pos depth aim] [d u]]
                 (prn [d u] [pos depth aim])
                 (case d
                   :forward [(+ pos u) (+ depth (* aim u)) aim]
                   :down [pos depth (+ aim u)]
                   :up [pos depth (- aim u)]
                   [pos depth aim]))
               [0 0 0]
               parsed-data)
       drop-last
       (apply *)))

(comment
  (get-result-2 parsed-sample-data)
  ;; => 900
  (get-result-2 parsed-real-data)
  ;; => 2044620088
  )
