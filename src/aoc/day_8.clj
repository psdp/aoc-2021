(ns aoc.day-8
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def sample-input "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce")

(def digit-patterns
  {0 #{\a \b \c \e \f \g}
   1 #{\c \f}
   2 #{\a \c \d \e \g}
   3 #{\a \c \d \f \g}
   4 #{\b \c \d \f}
   5 #{\a \b \d \f \g}
   6 #{\a \b \d \e \f \g}
   7 #{\a \c \f}
   8 #{\a \b \c \d \e \f \g}
   9 #{\a \b \c \d \f \g}})

(defn parse-input [input]
  (->> input
       str/split-lines
       (map (fn [l]
              (-> l
                  (str/split #"\|")
                  (->> (map #(re-seq #"\w+" %))))))))

(def parsed-sample-input (parse-input sample-input))

(def parsed-real-input (parse-input (slurp "input/day-8.txt")))

(defn part-1 [input]
  (->> input
       (map second)
       (map (fn [x]
              (filter #(#{2 3 4 7} (count %)) x)))
       (map count)
       (apply +)))

(comment
  (part-1 parsed-sample-input) ;; => 26
  (part-1 parsed-real-input) ;; => 476
  )

;; (cycle [1000 100 10 1])
