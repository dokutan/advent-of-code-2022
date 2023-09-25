(ns aoc2022.day06
  (:require [clojure.java.io :as io]))

(defn first-distinct-n [n]
  (fn [data]
    (loop [data data counter n]
      (let [packet (take n data)]
        (if (apply distinct? packet)
          counter
          (recur (rest data) (inc counter)))))))

(println "day 06")
(let [data (->> "day06"
                (io/resource)
                (slurp)
                (char-array)
                (vec))]

  ;; part 1: 1134
  (println ((first-distinct-n 4) data))

  ;; part 2: 2263
  (println ((first-distinct-n 14) data)))
