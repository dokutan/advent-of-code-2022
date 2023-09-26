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

(defn part1
  "Solve part 1."
  [sensors-beacons]
  (let [row
        (loop [row {} sensors-beacons sensors-beacons]
          (let [s-b
                (first sensors-beacons)

                distance ; between sensor and beacon
                (manhattan-distance
                 [(first s-b) (second s-b)]
                 [(get s-b 2) (get s-b 3)])

                row
                (reduce
                 merge
                 row
                 (for [x (range (- (first s-b) distance) (+ (first s-b) distance)) y [2000000]]
                   (if (<= (manhattan-distance [(first s-b) (second s-b)] [x y]) distance)
                     {[x y] :blocked}
                     {})))]

            (if (= 1 (count sensors-beacons))
              row
              (recur row (rest sensors-beacons)))))

        row ;add sensors and beacons
        (reduce merge
                row
                (map (fn [[sx sy bx by]] {[sx sy] :sensor [bx by] :beacon})
                     sensors-beacons))]

    (count (filter (fn [k] (and (= 2000000 (second (first k))) (= :blocked (second k)))) row))))

(println "day 15")
(let [sensors-beacons ; [[sensor-x sensor-y beacon-x beacon-y] ...]
      (->> "day15"
           io/resource
           (slurp)
           string/split-lines
           (mapv parse-line))]

  ;; part 1: 6275922
  (print (part1 sensors-beacons)))
