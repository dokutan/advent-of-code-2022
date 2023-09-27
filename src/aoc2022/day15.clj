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

(defn part2
  "Solve part2."
  [sensors-beacons]

  (let [boundaries ; of the scanned areas
        (map
         (fn [s-b]
           (let [x (first s-b)
                 y (second s-b)
                 radius
                 (manhattan-distance
                  [x y]
                  [(get s-b 2) (get s-b 3)])]
             [(+ (- y x) radius 1)
              (- (- y x radius) 1)
              (+ x y radius 1)
              (- (+ x y) radius 1)]))
         sensors-beacons)

        boundaries+ ; with gradient +1
        (flatten
         (map
          (fn [[a b _ _]] [a b])
          boundaries))

        boundaries- ; with gradient -1
        (flatten
         (map
          (fn [[_ _ a b]] [a b])
          boundaries))

        intersections
        (for [b+ boundaries+ b- boundaries-]
          ; (b-a)/2 , (a+b)/2
          [(/ (- b- b+) 2)
           (/ (+ b+ b-) 2)])

        intersections ; that are not blocked
        (for [[ix iy] intersections]
          (let [not-blocked?
                (reduce
                 (fn [a b] (and a b))
                 (for [[sx sy bx by] sensors-beacons]
                   (let [radius (manhattan-distance [sx sy] [bx by])
                         distance (manhattan-distance [sx sy] [ix iy])]
                     (>= distance radius))))]
            (if (and
                 not-blocked?
                 (>= ix 0)
                 (>= iy 0)
                 (<= ix 4000000)
                 (<= iy 4000000))
              [ix iy]
              nil)))

        beacon
        (first (filter identity intersections))]

    (+ (* 4000000 (first beacon)) (second beacon))))

(println "day 15")
(let [sensors-beacons ; [[sensor-x sensor-y beacon-x beacon-y] ...]
      (->> "day15"
           io/resource
           (slurp)
           string/split-lines
           (mapv parse-line))]

  ;; part 1: 6275922
  (println (part1 sensors-beacons))

  ;; part 2: 11747175442119
  (println (part2 sensors-beacons)))
