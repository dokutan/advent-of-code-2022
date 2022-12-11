(ns aoc2022.day11
  (:require [clojure.string :as string]
            [clojure.java.io :as io]))

(defn parse-monkey [monkey]
  (let [lines (string/split-lines monkey)

        items (read-string
               (str
                "["
                (string/replace (second lines) "  Starting items: " "")
                "]"))

        operation (as-> (nth lines 2) v
                    (string/replace v "  Operation: new = " "")
                    (string/split v #" ")
                    (str "(fn [old] (" (second v) " " (first v) " " (last v) "))")
                    (read-string v)
                    (eval v))


        divisible-by (string/replace (nth lines 3) "  Test: divisible by " "")
        next-true (string/replace (nth lines 4) "    If true: throw to monkey " "")
        next-false (string/replace (nth lines 5) "    If false: throw to monkey " "")
        throw-to (eval
                  (read-string
                   (str "(fn [lvl] (if (zero? (mod lvl " divisible-by ")) " next-true " " next-false "))")))]

    {:items items
     :operation operation
     :throw-to throw-to}))

(println "day 11")
(let [monkeys (as-> "day11" v
                (io/resource v)
                (slurp v)
                (string/split v #"\n\n")
                (mapv parse-monkey v))

      inspections
      (loop [monkeys monkeys
             current-monkey 0
             round 0
             inspections {}]

        (if (= round 20)
          ;(map #(:items %) monkeys)
          inspections

          (if-not (zero? (count (:items (nth monkeys current-monkey))))
            ;; inspect one item
            (let [;; get item from current monkey
                  monkey (nth monkeys current-monkey)
                  items (:items monkey)
                  monkey (assoc monkey :items (vec (rest items)))
                  monkeys (assoc monkeys current-monkey monkey)
                  item (first items)
                  item ((:operation monkey) item)
                  item (/ (- item (mod item 3)) 3)

                  ;; throw item
                  throw-to ((:throw-to monkey) item)
                  monkey (nth monkeys throw-to)
                  items (conj (:items monkey) item)
                  monkey (assoc monkey :items items)
                  monkeys (assoc monkeys throw-to monkey)

                  inspections (assoc
                               inspections
                               current-monkey
                               (inc (get inspections current-monkey 0)))]

              (recur monkeys current-monkey round inspections))

            ;; next monkey
            (let [current-monkey (inc current-monkey)
                  current-monkey (mod current-monkey (count monkeys))
                  round (if (zero? current-monkey) (inc round) round)]
              (recur monkeys current-monkey round inspections)))))]

  ;; part1: 55930
  (println (apply * (take 2 (sort > (vals inspections))))))