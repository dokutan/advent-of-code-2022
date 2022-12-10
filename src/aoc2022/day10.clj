(ns aoc2022.day10
  (:require [clojure.string :as string])
  (:require [clojure.java.io :as io]))

(defn parse-instruction [instruction]
  (if (= instruction "noop")
    nil
    (read-string (string/replace-first instruction "addx " ""))))

(defn compute-x [instructions]
  (loop [instructions instructions
         x [1]]

    (let [instruction (first instructions)
          instructions (next instructions)
          x (conj x (last x))
          x (if instruction
              (conj x (+ (last x) instruction))
              x)]

      (if instructions
        (recur instructions x)
        x))))

(println "day 10")
(let [instructions (->> "day10"
                        (io/resource)
                        (slurp)
                        (string/split-lines)
                        (map parse-instruction))
      x (compute-x instructions)
      samples [20 60 100 140 180 220]
      signal-strengths (map *
                            samples
                            (map #(nth x (dec %)) samples))]

  ;; part 1: 11720
  (println (reduce + signal-strengths))

  ;; part 2: ERCREPCJ
  (println
   (string/join
    "\n"
    (map #(apply str %)
         (partition
          40
          (map #(if (<= (abs (- (mod % 40) (nth x %))) 1) "█" " ")
               (range 240)))))))
