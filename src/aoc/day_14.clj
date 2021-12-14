(ns aoc.day-14
  (:require [clojure.string :as str]))

(def sample-input "NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C")

(defn parse-input [input]
  (let [[input rules] (str/split input #"\R\R")
        rules (->> rules
                   str/split-lines
                   (map (fn [line]
                          (let [[_ pair insertion] (re-find #"(\w+) -> (\w+)" line)]
                            [(seq pair) insertion])))
                   (into {}))]
    {:input input
     :rules rules}))

(def parsed-sample-input (parse-input sample-input))

(def parsed-real-input (parse-input (slurp "input/day-14.txt")))

(defn step-iterate [rules]
  (fn [input]
    (let [pairs (partition 2 1 input)
          insertions (map rules pairs)]
      (-> (interleave input insertions)
          vec
          (conj (last input))
          (->> (apply str))))))

(defn get-result [{:keys [input rules]} pos]
  (-> (iterate (step-iterate rules) input)
      (nth pos)
      frequencies
      vals
      (->> (apply (juxt max min))
           (apply -))))

(comment
  (get-result parsed-sample-input 10) ;; => 1588
  (get-result parsed-real-input 10) ;; => 3247
  )
