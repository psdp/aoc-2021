(ns aoc.day-10
  (:require [clojure.string :as str]
            [aoc.util :as util]))

(def sample-input
  "[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]")

(def parsed-sample-input (str/split-lines sample-input))

(def parsed-real-input (util/seq-input-data 10))

(def paren-pairs
  {\( \), \[ \], \{ \}, \< \>})

(def illegal-char-points
  {\) 3, \] 57, \} 1197, \> 25137})

(defn match-paren? [opening closing]
  (= (get paren-pairs opening) closing))

(defn find-illegal-char [input]
  (loop [[fst & rst] input
         openings '()]
    (cond
      (nil? fst)
      nil

      (contains? paren-pairs fst)
      (recur rst (conj openings fst))

      :else
      (let [[opening openings'] ((juxt peek pop) openings)]
        (if (match-paren? opening fst)
          (recur rst openings')
          fst)))))

(defn calc-points [input]
  (->> input
       (keep find-illegal-char)
       (map illegal-char-points)
       (reduce +)))

(comment
  (calc-points parsed-sample-input) ;; => 26397
  (calc-points parsed-real-input) ;; => 442131
  )
