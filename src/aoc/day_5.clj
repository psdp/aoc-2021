(ns aoc.day-5
  (:require [clojure.string :as str]))

(def sample-input
  "0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2")

(defn parse-input [input]
  (->> input
       str/split-lines
       (map #(->> (re-matches #"^(\d+),(\d+) -> (\d+),(\d+)$" %)
                  next
                  (map #(Integer/parseInt %))
                  (partition 2)))))

(def parsed-sample-input (parse-input sample-input))

(def parsed-real-input (parse-input (slurp "input/day-5.txt")))

(defn hori-or-vert-line? [[x1 y1] [x2 y2]]
  (or (= x1 x2) (= y1 y2)))

(defn get-points [[x1 y1 :as a] [x2 y2 :as b]]
  (let [get-range (fn [a b]
                    (if (> b a)
                      (range a (inc b))
                      (range a (dec b) -1)))]
    (if (hori-or-vert-line? a b)
      (for [x (get-range x1 x2)
            y (get-range y1 y2)]
        [x y])
      (map vector (get-range x1 x2) (get-range y1 y2)))))

(defn inc2 [x]
  (inc (or x 0)))

(defn fill-diagram
  ([input]
   (fill-diagram (constantly true) input))
  ([pred input]
   (->> input
        (reduce (fn [m [a b]]
                  (if (pred a b)
                    (reduce #(update %1 %2 inc2) m (get-points a b))
                    m)) {}))))

(defn get-result
  ([]
   (get-result nil))
  ([pred]
   (fn [input]
     (let [fill-diagram (if pred
                          (partial fill-diagram pred)
                          fill-diagram)]
       (->> input
            fill-diagram
            vals
            (filter #(> % 1))
            count)))))

(def get-result-1
  (get-result hori-or-vert-line?))

(comment
  (get-result-1 parsed-sample-input) ;; => 5
  (get-result-1 parsed-real-input) ;; => 6113
  )

(def get-result-2
  (get-result))

(comment
  (get-result-2 parsed-sample-input) ;; => 12
  (get-result-2 parsed-real-input) ;; => 20373
  )
