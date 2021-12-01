(ns aoc.util
  (:require [clojure.string :as str]))

(defn seq-input-data
  ([day]
   (seq-input-data day nil))
  ([day f]
   (-> (str "input/day-" day ".txt")
       slurp
       str/split-lines
       (cond->> f
         (mapv #(f %))))))
