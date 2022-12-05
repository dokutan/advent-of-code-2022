(ns aoc2022.day05
  (:require [clojure.string :as string])
  (:require [clojure.java.io :as io]))


;; manually extracted from the input
(def stacks
  {1 ["W" "B" "D" "N" "C" "F" "J"]
   2 ["P" "Z" "V" "Q" "L" "S" "T"]
   3 ["P" "Z" "B" "G" "J" "T"]
   4 ["D" "T" "L" "J" "Z" "B" "H" "C"]
   5 ["G" "V" "B" "J" "S"]
   6 ["P" "S" "Q"]
   7 ["B" "V" "D" "F" "L" "M" "P" "N"]
   8 ["P" "S" "M" "F" "B" "D" "L" "R"]
   9 ["V" "D" "T" "R"]})

(defn parse-move [line]
  (let [[_ amount _ from _ to] (string/split line #" ")]
    {:amount (Integer/parseInt amount)
     :from (Integer/parseInt from)
     :to (Integer/parseInt to)}))

(defn move-1 [stacks amount from to]
  (let [from-stack (get stacks from)
        to-stack (get stacks to)
        to-stack (vec (concat to-stack (take amount (reverse from-stack))))
        from-stack (vec (drop-last amount from-stack))
        stacks (dissoc stacks from to)
        stacks (assoc stacks from from-stack to to-stack)]
    stacks))

(defn move-2 [stacks amount from to]
  (let [from-stack (get stacks from)
        to-stack (get stacks to)
        to-stack (vec (concat to-stack (reverse (take amount (reverse from-stack)))))
        from-stack (vec (drop-last amount from-stack))
        stacks (dissoc stacks from to)
        stacks (assoc stacks from from-stack to to-stack)]
    stacks))

(defn print-stacks [stacks]
  (println (str
            (last (get stacks 1))
            (last (get stacks 2))
            (last (get stacks 3))
            (last (get stacks 4))
            (last (get stacks 5))
            (last (get stacks 6))
            (last (get stacks 7))
            (last (get stacks 8))
            (last (get stacks 9)))))

(println "day 05")
(let [moves (->> "day05"
                 (io/resource)
                 (slurp)
                 (string/split-lines)
                 (filter #(string/starts-with? % "move "))
                 (map parse-move))
      apply-moves-1 (concat
                     '(-> stacks)
                     (map #(list 'move-1 (:amount %) (:from %) (:to %)) moves))
      apply-moves-2 (concat
                     '(-> stacks)
                     (map #(list 'move-2 (:amount %) (:from %) (:to %)) moves))]

  ;; part 1: LBLVVTVLP
  (print-stacks (eval apply-moves-1))

  ;; part 2: TPFFBDRJD
  (print-stacks (eval apply-moves-2)))
