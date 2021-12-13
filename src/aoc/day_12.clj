(ns aoc.day-12
  (:require [clojure.string :as str]))

(def sample-input "start-A
start-b
A-c
A-b
b-d
A-end
b-end")

(defn parse-input [input]
  (->> input
       str/split-lines
       (map #(->> %
                  (re-matches #"(\w+)-(\w+)")
                  next))))

(def parsed-sample-input (parse-input sample-input))

(def parsed-real-input (parse-input (slurp "input/day-12.txt")))

(defn find-directions [points]
  (->> points
       (reduce (fn [dir [start end]]
                 (-> dir
                     (update start (fnil conj #{}) end)
                     (update end (fnil conj #{}) start))) {})))

(defn small-cave? [s]
  (every? #(Character/isLowerCase %) s))

(defn find-paths [input visited-fn]
  (let [directions (find-directions input)]
    (loop [[path & paths] [["start"]]
           res            #{}]
      (if path
        (let [end (peek path)]
          (if (= end "end")
            (recur paths (conj res path))
            (let [visited    (visited-fn path)
                  next-paths (->> (get directions end)
                                  (remove (set visited)))
                  paths'     (into paths (map #(conj path %) next-paths))]
              (recur paths' res))))
        res))))

(defn filter-small-cave [path]
  (filter small-cave? path))

(defn get-result-1 [input]
  (count (find-paths input filter-small-cave)))

(comment
  (get-result-1 parsed-sample-input) ;; => 10
  (get-result-1 parsed-real-input) ;; => 3738
  )

;;; part 2

(defn visited-twice [path]
  (let [visited (filter-small-cave path)]
    (as-> (frequencies visited) $freqs
      (if (every? #(= 1 %) (vals $freqs))
        #{"start"}
        visited))))

(defn get-result-2 [input]
  (count (find-paths input visited-twice)))

(comment
  (get-result-2 parsed-sample-input) ;; => 36
  (get-result-2 parsed-real-input) ;; => 120506
  )
