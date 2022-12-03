(ns aoc2022.day03
  (:require [clojure.string :as string])
  (:require [clojure.set :as set])
  (:require [clojure.java.io :as io]))

(defn split-halves [items]
  (let [half (/ (count items) 2)]
    [(set (take half items))
     (set (take-last half items))]))

(defn char->priority [item]
  (let [i (int item)]
    (if (>= i 97)
      (- i 96)
      (- i 38))))

(println "day 03")
(let [items (->> "day03"
                 (io/resource)
                 (slurp)
                 (string/split-lines)
                 (map #(map char %)))
      shared-items (map (comp char->priority
                              first
                              #(apply set/intersection %)
                              split-halves)
                        items)
      groups (partition 3 items)
      badges (map (comp char->priority
                        first
                        #(apply set/intersection %)
                        #(map set %))
                  groups)]

  ;; part 1: 8298
  (println (reduce + shared-items))

  ;; part 1: 2708
  (println (reduce + badges)))
