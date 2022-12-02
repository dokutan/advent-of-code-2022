(ns aoc2022.day01
  (:require [clojure.string :as string])
  (:require [clojure.java.io :as io]))

(println "day 01")
(let [sums (as-> "day01" v
             (io/resource v)
             (slurp v)
             (str "[[" v "]]")
             (string/replace v "\n\n" "][")
             (read-string v)
             (map #(reduce + %1) v))]

  ;; part 1: 69281
  (println (apply max sums))

  ;; part 2: 201524
  (println (->> sums
                (sort >)
                (take 3)
                (reduce +))))
