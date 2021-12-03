(ns aoc.day-3
  (:require [clojure.string :as str]
            [aoc.util :as util]))

(def sample-input "00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010")

(defn parse-input-1 [input]
  (loop [[x & xs] input
         result []]
    (if-not x
      (for [col result]
        (->> (for [[k items] (group-by identity col)
                   :let [cnt (count items)]]
               [k cnt])
             (sort-by second >)
             ffirst))
      (recur xs
             (reduce (fn [res [k x]]
                       (update res k conj x))
                     result
                     (map-indexed vector x))))))

(def parsed-sample-input
  (str/split-lines sample-input))

(def parsed-real-input
  (util/seq-input-data 3))

(def parse-bin-str
  (comp read-string #(apply str "2r" %)))

(defn get-result [input]
  (prn input)
  (let [input-reversed (map {\0 \1 \1 \0} input)]
    (* (parse-bin-str input)
       (parse-bin-str input-reversed))))

(comment
  (get-result (parse-input-1 parsed-sample-input)) ;; => 198
  (get-result (parse-input-1 parsed-real-input)) ;; => 3687446
  )

;;; part 2

(defn find-last [sort-fn]
  (fn [input]
    (loop [remaining input
           pos 0]
      (if (= (count remaining) 1)
        (first remaining)
        (let [c (->> (map #(nth % pos) remaining)
                     (group-by identity)
                     (map (fn [[k items]]
                            [k (count items)]))
                     (sort sort-fn)
                     ffirst)
              remaining (filter #(= (nth % pos) c) remaining)]
          (recur remaining (inc pos)))))))

(def find-oxygen
  (find-last (fn [[a cnt-a] [b cnt-b]]
               (if (= cnt-a cnt-b)
                 (= a \1)
                 (compare cnt-b cnt-a)))))

(def find-scrubber
  (find-last (fn [[a cnt-a] [b cnt-b]]
               (if (= cnt-a cnt-b)
                 (= a \0)
                 (compare cnt-a cnt-b)))))

(defn get-result-2 [input]
  (* (parse-bin-str (find-oxygen input))
     (parse-bin-str (find-scrubber input))))

(comment
  (get-result-2 parsed-sample-input) ;; => 230
  (get-result-2 parsed-real-input) ;; => 4406844
  )
