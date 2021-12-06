(ns aoc.day-6)

(def sample-input "3,4,3,1,2")

(defn parse-input [input]
  (->> input
       (re-seq #"\d+")
       (map #(Integer/parseInt %))))

(def parsed-sample-input (parse-input sample-input))

(def parsed-real-input (parse-input (slurp "input/day-6.txt")))

(def reset-val 6)
(def init-val 8)

(defn get-result [input day]
  (-> (frequencies input)
      (->> (iterate (fn [state]
                      (->> state
                           (reduce (fn [res [num cnt]]
                                     (if (zero? num)
                                       (-> res
                                           (update reset-val (fnil + 0) cnt)
                                           (update init-val (fnil + 0) cnt)
                                           (update num (fnil - 0) cnt))
                                       (-> res
                                           (update num (fnil - 0) cnt)
                                           (update (dec num) (fnil + 0) cnt))))
                                   state)))))
       (nth day)
       vals
       (->> (apply +))))

(comment
  (get-result parsed-sample-input 80) ;; => 5934
  (get-result parsed-real-input 80) ;; => 390011

  (get-result parsed-sample-input 256) ;; => 26984457539
  (get-result parsed-real-input 256) ;; => 1746710169834
  )
