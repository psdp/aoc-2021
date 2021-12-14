(ns aoc.day-13
  (:require [clojure.string :as str]))

(def sample-input "6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5")

(defn parse-input [input]
  (let [[coords-input instructions-input]
        (str/split input #"\R\R")

        coords (->> (str/split-lines coords-input)
                    (map (fn [line]
                           (->> (re-seq #"\d+" line)
                               (map #(Integer/parseInt %))))))

        instructions (->> (str/split-lines instructions-input)
                          (map (fn [line]
                                 (let [[_ direction pos] (re-find #"(\w+)=(\d+)" line)]
                                   [(keyword direction)
                                    (Integer/parseInt pos)]))))]
    {:coords coords
     :instructions instructions}))

(def parsed-sample-input (parse-input sample-input))

(def parsed-real-input (parse-input (slurp "input/day-13.txt")))

(defn coords->paper [coords]
  (let [width (->> coords
                   (map first)
                   (apply max)
                   inc)
        height (->> coords
                   (map second)
                   (apply max)
                   inc)
        blank (vec (for [_ (range height)]
                     (vec (for [_ (range width)]
                            \.))))]
    (->> coords
         (reduce (fn [paper [x y]]
                   (assoc-in paper [y x] \#))
                 blank))))

(defn fold [paper direction pos]
  (let [resize #(vec (take pos %))
        cut #(vec (reverse (drop (inc pos) %)))
        merge (fn [new cutoff pos]
                (let [distance (- (count new) (count cutoff))]
                  (->> (map-indexed vector cutoff)
                       (reduce (fn [chart [idx val]]
                                 (let [pos (+ distance idx)]
                                   (if (nth chart pos)
                                     (if (vector? val)
                                       (->> (map-indexed vector val)
                                            (reduce (fn [chart [i v]]
                                                      (cond-> chart
                                                        (= v \#)
                                                        (assoc-in [pos i] v)))
                                                    chart))
                                       (cond-> chart
                                         (= val \#)
                                         (assoc pos val)))
                                     chart)))
                               new))))
        fold-y? (= direction :y)
        split (if fold-y?
                (juxt resize cut)
                (juxt #(mapv resize %) #(mapv cut %)))
        [paper cutoff] (split paper)]
    (if fold-y?
      (merge paper cutoff pos)
      (mapv #(merge %1 %2 pos) paper cutoff))))

(defn count-dots [paper]
  (->> paper
       flatten
       (filter #(= \# %))
       count))

(defn get-result-1 [{:keys [coords instructions]}]
  (let [paper (coords->paper coords)
        [direction pos] (first instructions)]
    (-> (fold paper direction pos)
        count-dots)))

(defn get-result-2 [{:keys [coords instructions]}]
  (let [paper (coords->paper coords)]
    (->> instructions
         (reduce #(fold %1 (first %2) (second %2)) paper))))

(defn prn-tbl [paper]
  (doseq [row paper]
    (doseq [v row]
      (print v))
    (println)))

(comment
  (get-result-1 parsed-sample-input) ;; => 17
  (get-result-1 parsed-real-input) ;; => 781

  (get-result-2 parsed-sample-input)
  (prn-tbl (get-result-2 parsed-real-input))
  )
