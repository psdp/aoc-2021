(ns aoc.day-4
  (:require [clojure.string :as str]))

(def sample-input
  "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7")

(defn parse-input [input]
  (let [[draws-raw & boards-raw] (str/split input #"\R\R")
        draws (->> (re-seq #"\d+" draws-raw)
                   (map #(Integer/parseInt %)))
        boards (->> boards-raw
                    (map #(->> (str/split-lines %)
                               (mapv #(->> (re-seq #"\d+" %)
                                           (mapv #(Integer/parseInt %)))))))]
    {:draws draws
     :boards boards}))

(def parsed-sample-input (parse-input sample-input))

(def parsed-real-input (parse-input (slurp "input/day-4.txt")))

(defn transpose [coll]
  (apply mapv vector coll))

(defn board-complete? [board draws]
  (let [complete? (fn [x]
                      (some? (some #(every? draws %) x)))]
    (or (complete? board)
        (complete? (transpose board)))))

(defn get-score [board draws last-draw-num]
  (let [nums (apply concat board)
        excludes (remove draws nums)
        sum (apply + excludes)]
    (* sum last-draw-num)))

(defn get-result [{:keys [draws boards] :as input}]
  (let [[current-draws remaining-draws] ((juxt #(set (take 5 %)) #(drop 5 %)) draws)
        last-draw-num (nth draws 4)]
    (loop [draws current-draws
           [next-draw-num & next-remaining] remaining-draws
           last-draw-num last-draw-num]
      (if-let [complete-board (some #(when (board-complete? % draws) %) boards)]
        (get-score complete-board draws last-draw-num)
        (when next-draw-num
          (recur (conj draws next-draw-num)
                 next-remaining
                 next-draw-num))))))

(comment
  (get-result parsed-sample-input) ;; => 4512
  (get-result parsed-real-input) ;; => 35711
  )

;;; part 2

(defn get-result-2 [{:keys [draws boards] :as input}]
  (let [[current-draws remaining-draws] ((juxt #(set (take 5 %)) #(drop 5 %)) draws)
        last-draw-num (nth draws 4)]
    (loop [draws current-draws
           [next-draw-num & next-remaining] remaining-draws
           last-draw-num last-draw-num
           boards boards
           last-board-draws-sum nil]
      (if (and next-draw-num (seq boards))
        (let [[last-complete-board boards]
              ((juxt (fn [b] (some #(when (board-complete? % draws) %) b))
                     (fn [b] (remove #(board-complete? % draws) b)))
               boards)
              last-board-draws-sum (if last-complete-board
                                     (get-score last-complete-board draws last-draw-num)
                                     last-complete-board)]
          (recur (conj draws next-draw-num)
                 next-remaining
                 next-draw-num
                 boards
                 last-board-draws-sum))
        last-board-draws-sum))))

(comment
  (get-result-2 parsed-sample-input) ;; => 1924
  (get-result-2 parsed-real-input) ;; => 5586
  )
