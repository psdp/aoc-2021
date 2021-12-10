(ns aoc.day-9
  (:require [clojure.string :as str]))

(def sample-input
  "2199943210
3987894921
9856789892
8767896789
9899965678")

(defn parse-input [input]
  (->> input
       str/split-lines
       (mapv #(->> (re-seq #"\d" %)
                  (mapv #(Integer/parseInt %))))))

(def parsed-sample-input (parse-input sample-input))

(def parsed-real-input (parse-input (slurp "input/day-9.txt")))

(defn adjacents [[x y]]
  [[(dec x) y]
   [(inc x) y]
   [x (dec y)]
   [x (inc y)]])

(defn find-lowpoints [input]
  (for [[i row] (map-indexed vector input)
        [j n] (map-indexed vector row)
        :when (->> (adjacents [i j])
                   (keep #(get-in input %))
                   (every? #(> % n)))]
    n))

(defn get-score [input]
  (->> input
       find-lowpoints
       (map inc)
       (apply +)))


(comment
  (get-score parsed-sample-input)
  (get-score parsed-real-input))
