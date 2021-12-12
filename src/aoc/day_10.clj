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

(defn check-syntax [input & [autocomplete?]]
  (loop [[fst & rst] input
         openings '()]
    (cond
      (nil? fst)
      (if autocomplete?
        (map paren-pairs openings)
        nil)

      (contains? paren-pairs fst)
      (recur rst (conj openings fst))

      :else
      (let [[opening openings'] ((juxt peek pop) openings)]
        (if (match-paren? opening fst)
          (recur rst openings')
          (if autocomplete?
            nil
            fst))))))

(def find-illegal-char #(check-syntax %))

(defn score-1 [input]
  (->> input
       (keep find-illegal-char)
       (map illegal-char-points)
       (reduce +)))

(comment
  (score-1 parsed-sample-input) ;; => 26397
  (score-1 parsed-real-input) ;; => 442131
  )

;;; part 2

(def autocomplete #(check-syntax % true))

(def char-points
  {\) 1, \] 2, \} 3, \> 4})

(defn median [coll] (nth (sort coll) (/ (count coll) 2)))

(defn score-2 [input]
  (->> input
       (keep autocomplete)
       (map (fn [chars]
              (->> chars
                   (map char-points)
                   (reduce #(+ (* %1 5) %2)))))
       median))

(comment
  (score-2 parsed-sample-input) ;; => 288957
  (score-2 parsed-real-input) ;; => 3646451424
  )
