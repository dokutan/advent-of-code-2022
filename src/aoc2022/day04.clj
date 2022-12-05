(ns aoc2022.day04
  (:require [clojure.string :as string])
  (:require [clojure.set :as set])
  (:require [clojure.java.io :as io]))

(defn parse-section [section]
  (let [bounds (map read-string
                    (string/split section #"-"))
        lower (first bounds)
        upper (inc (last bounds))]
    (set (range lower upper))))

(defn superset-or-subset? [set1 set2]
  (or (set/subset? set1 set2)
      (set/superset? set1 set2)))

(println "day 04")
(let [pairs (->> "day04"
                 (io/resource)
                 (slurp)
                 (string/split-lines)
                 (map #(string/split % #","))
                 ;(map #(map (fn [r] (string/split r #"-")) %) )
                 (map #(map parse-section %)))
      pairs-contained (filter #(apply superset-or-subset? %) pairs)
      pairs-overlap (map #(if (zero? (count (apply set/intersection %))) 0 1) pairs)]

  ;; part 1: 526
  (println (count pairs-contained))

  ;; part2: 886
  (println (reduce + pairs-overlap)))
