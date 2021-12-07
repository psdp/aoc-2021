(ns aoc.day-7)

(def sample-input "16,1,2,0,4,2,7,1,2,14")

(defn parse-input [input]
  (->> input
       (re-seq #"\d+")
       (map #(Integer/parseInt %))))

(def parsed-sample-data (parse-input sample-input))

(def parsed-real-data (parse-input (slurp "input/day-7.txt")))

(defn median [ns]
  (let [ns (sort ns)
        cnt (count ns)
        mid (bit-shift-right cnt 1)]
    (if (odd? cnt)
      (nth ns mid)
      (/ (+ (nth ns mid) (nth ns (dec mid))) 2))))

(defn avg [numbers]
    (/ (apply + numbers) (count numbers)))

(defn calc-fuel-consumption [data target cost-fn]
  (reduce (fn [total n]
            (let [cost (cost-fn n target)]
              (+ total cost)))
          0
          data))

(defn cost-fn-1 [n target]
  (Math/abs (- n target)))

(defn get-result-1 [data]
  (let [target (median data)]
    (calc-fuel-consumption data target cost-fn-1)))

(comment
  (get-result-1 parsed-sample-data) ;; => 37
  (get-result-1 parsed-real-data) ;; => 364898
  )

(defn cost-fn-2 [n target]
  (as-> (Math/abs (- n target)) $
    (/ (* $ (inc $)) 2)))

(defn get-result-2 [data]
  (let [target (int (Math/ceil (avg data)))
        target-dec (dec target)]
    (->> [target target-dec]
         (mapv #(calc-fuel-consumption data % cost-fn-2))
         sort
         first)))

(comment
  (get-result-2 parsed-sample-data) ;; => 168
  (get-result-2 parsed-real-data) ;; => 104149091
  )
