(ns aoc2022.day15
  (:require [clojure.string :as string])
  (:require [clojure.java.io :as io]))

(defn parse-line
  "Parse one line of the input."[line]
  (->>
   (re-matches
    #"Sensor at x=([-0-9]+), y=([-0-9]+): closest beacon is at x=([-0-9]+), y=([-0-9]+)"
    line)
   rest
   (mapv (fn [x] (Integer/parseInt x)))))

(defn manhattan-distance [p q]
  (+
   (abs (- (first p) (first q)))
   (abs (- (second p) (second q)))))

(defn blocked-range-on-y
  "Calculate the range of x that is blocked by a sensor and `distance`.
   Returns [x-min x-max] or []."
  [y sensor-x sensor-y distance]
  (let [y-distance (abs (- sensor-y y))
        x-distance (- distance y-distance)]
    (if (> y-distance distance)
      []
      [(- sensor-x x-distance) (+ sensor-x x-distance)])))

(defn part1
  "Solve part 1."
  [sensors-beacons]
  (let [blocked-ranges
        (map
         (fn [s-b]
           (blocked-range-on-y
            2000000
            (first s-b)
            (second s-b)
            (manhattan-distance
             [(first s-b) (second s-b)]
             [(get s-b 2) (get s-b 3)])))
         sensors-beacons)]

    ;; total length blocked
    (+ (abs (apply min (flatten blocked-ranges)))
       (apply max (flatten blocked-ranges)))))

(println "day 15")
(let [sensors-beacons ; [[sensor-x sensor-y beacon-x beacon-y] ...]
      (->> "day15"
           io/resource
           (slurp)
           string/split-lines
           (mapv parse-line))]

  ;; part 1: 6275922
  (println (part1 sensors-beacons)))
