(ns aoc2022.day02
  (:require [clojure.string :as string])
  (:require [clojure.java.io :as io]))

(defn shape->int [shape]
  (condp = shape
    "A" 1
    "B" 2
    "C" 3
    "X" 1
    "Y" 2
    "Z" 3))

(defn result-points [shape1 shape2]
  (cond
    (= shape1 shape2) 3
    (or
     (= [1 3] [shape1 shape2])
     (= [2 1] [shape1 shape2])
     (= [3 2] [shape1 shape2])) 0
    :else 6))

(defn result-shape
  "The second shape required to achieve `result` wiht `shape1`"
  [[shape1 result]]
  (cond
    (= 2 result) shape1
    (= 1 result)
    (condp = shape1
      1 3
      2 1
      3 2)
    :else
    (condp = shape1
      1 2
      2 3
      3 1)))

(println "day 02")
(let [games (as-> "day02" v
              (io/resource v)
              (slurp v)
              (string/split-lines v)
              (map #(string/split % #" ") v)
              (map #(map shape->int %) v))
      game-points-1 (map #(+ (apply result-points %1)
                             (nth %1 1))
                         games)
      points-1 (apply + game-points-1)
      game-points-2 (map #(+ (result-shape %1)
                             (condp = (nth %1 1)
                               1 0
                               2 3
                               3 6))
                         games)
      points-2 (apply + game-points-2)]
  
  ;; part 1: 13268
  (println points-1)
  
  ;; part 2: 15508
  (println points-2))
