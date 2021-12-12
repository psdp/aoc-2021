(ns aoc.day-11
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def sample-input "5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526")

(defn parse-input [input]
  (->> input
       str/split-lines
       (mapv (fn [line]
               (->> (re-seq #"\d" line)
                    (mapv #(Integer/parseInt %)))))))

(def parsed-sample-input (parse-input sample-input))

(def parsed-real-input (parse-input (slurp "input/day-11.txt")))

(defn adjacents [[x y]]
  (let [expand (juxt dec identity inc)]
    (for [x' (expand x)
          y' (expand y)
          :when (not= [x y] [x' y'])]
      [x' y'])))

(defn find-zero-coords [input]
  (set (for [[i row] (map-indexed vector input)
             [j n] (map-indexed vector row)
             :when (zero? n)]
         [i j])))

(defn step-fn [input]
  (let [input' (->> input
                    (mapv (fn [row]
                            (mapv #(if (= % 9) 0 (inc %)) row))))]
    (loop [input input'
           zero-coords (find-zero-coords input)
           prev-zero-coords #{}]
      (let [next-zero-coords (set/difference zero-coords prev-zero-coords)]
        (if (seq next-zero-coords)
          (let [expanded (->> next-zero-coords
                              (map adjacents)
                              (apply concat))
                input' (->> expanded
                            (reduce (fn [input coord]
                                      (if-let [v (get-in input coord)]
                                        (assoc-in input coord (if (zero? v)
                                                                v
                                                                (as-> (inc v) $
                                                                  (if (> $ 9) 0 $))))
                                        input))
                                    input))
                zero-coords' (find-zero-coords input')]
            (recur input' zero-coords' zero-coords))
          input)))))

(defn count-flashes [input steps]
  (->> (iterate step-fn input)
       (take (inc steps))
       (map (comp count #(filter zero? %) flatten))
       (apply +)))

(comment
  (count-flashes parsed-sample-input 10) ;; => 204
  (count-flashes parsed-sample-input 100) ;; => 1656
  (count-flashes parsed-real-input 100) ;; => 1694
  )

;;; part 2

(defn find-first-step-all-flashes [input]
  (->> (iterate step-fn input)
       (map-indexed vector)
       (drop-while (fn [[_ x]]
                     (->> x
                          (apply concat)
                          (every? zero?)
                          not)))
       ffirst))

(comment
  (find-first-step-all-flashes parsed-sample-input) ;; => 195
  (find-first-step-all-flashes parsed-real-input) ;; => 346
  )
