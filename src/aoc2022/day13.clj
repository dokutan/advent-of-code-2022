(ns aoc2022.day13
  (:require [clojure.string :as string])
  (:require [clojure.java.io :as io]))

(def restv (comp vec rest))

(defn compare-2 [x y]
  (cond
    (and (number? x) (number? y))
    (compare x y)

    (= [] x y)
    0

    (and (vector? x) (vector? y))
    (let [compare-first (compare-2 (first x) (first y))]
      (if (zero? compare-first)
        (compare-2 (restv x) (restv y))
        compare-first))

    (and (vector? x) (number? y))
    (compare-2 x [y])

    (and (number? x) (vector? y))
    (compare-2 [x] y)

    (and (nil? x) (not (nil? y)))
    -1

    (and (not (nil? x)) (nil? y))
    1

    :else
    0))

(defn find-packet [packets packet]
  (first
   (filter
    identity
    (map #(if (= %1 packet) %2 nil)
         packets
         (iterate inc 1)))))

(println "day 13")
(let [pairs
      (as-> "day13" v
        (io/resource v)
        (slurp v)
        (str "[[" v "]]")
        (string/replace v "\n\n" "][")
        (read-string v))

      ordered-pairs
      (->> pairs
           (mapv #(compare-2 (first %) (second %)))
           (zipmap (iterate inc 1))
           (filter #(neg? (second %)))
           (keys))

      packets (apply concat pairs)
      packets (concat packets [[[2]] [[6]]])
      packets (sort compare-2 packets)

      divider-1 (find-packet packets [[2]])
      divider-2 (find-packet packets [[6]])]

  ;; part 1: 5292
  (println (reduce + ordered-pairs))
  
  ;; part 2: 23868
  (println (* divider-1 divider-2)))
