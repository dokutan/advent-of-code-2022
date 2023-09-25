(ns aoc2022.day14
  (:require
   [clojure.string :as string]
   [clojure.java.io :as io]
   [instaparse.core :as insta]))

(def line-parser
  (insta/parser
   "path = coordinates *(<' -> '> coordinates)
    coordinates = number <','> number
    number = *('0' / '1' / '2' / '3' / '4' / '5' / '6' / '7' / '8' / '9')"
   :input-format :abnf
   :output-format :hiccup))

(def line-transformation
  {:number (fn [& a] (Integer/parseInt (apply str a)))
   :coordinates (fn [x y] {:x x :y y})
   :path vector})

(defn draw-line
  "Draw a line of :rock in `grid` from `from` to `to`."
  [grid from to]
  (let [rocks (cond
                (and (= (:y from) (:y to))
                     (> (:x to) (:x from)))
                (->> (range (:x from) (inc (:x to)))
                     (map (fn [x] {:x x :y (:y from)})))

                (and (= (:y from) (:y to))
                     (< (:x to) (:x from)))
                (->> (range (:x from) (dec (:x to)) -1)
                     (map (fn [x] {:x x :y (:y from)})))

                (and (= (:x from) (:x to))
                     (> (:y to) (:y from)))
                (->> (range (:y from) (inc (:y to)))
                     (map (fn [y] {:x (:x from) :y y})))

                (and (= (:x from) (:x to))
                     (< (:y to) (:y from)))
                (->> (range (:y from) (dec (:y to)) -1)
                     (map (fn [y] {:x (:x from) :y y}))))]
    (reduce (fn [grid rock] (assoc grid [(:x rock) (:y rock)] :rock))
            grid
            rocks)))

(defn parse-line
  "Turn the lines of the input into a grid map."
  [input-line]
  (let [path (insta/transform line-transformation (line-parser input-line))]
    (loop [grid {} path path]
      (if (>= (count path) 2)
        (recur (draw-line grid (first path) (second path))
               (rest path))
        grid))))

(defn grid->string
  "Convert the `grid` map to a string"
  [grid]
  (string/join
   "\n"
   (for [y (range 0 180)]
     (apply
      str
      (for [x (range 450 600)]
        (condp = (get grid [x y])
          :rock "#"
          :sand "o"
          :source "+"
          "."))))))

(defn drop-sand
  "Drop one unit of sand from 500,0.
   Returns [grid settled?]"
  [grid]
  (let [sand-coordinates
        (loop [x 500 y 0]
          (cond
            (> y 200)
            nil

            (nil? (get grid [x (inc y)]))
            (recur x (inc y))

            (nil? (get grid [(dec x) (inc y)]))
            (recur (dec x) (inc y))

            (nil? (get grid [(inc x) (inc y)]))
            (recur (inc x) (inc y))

            :else
            [x y]))]

    (if sand-coordinates
      [(assoc grid sand-coordinates :sand) true]
      [grid false])))

(println "day 14")
(let [grid
      (->> "day14"
           (io/resource)
           (slurp)
           (string/split-lines)
           (map parse-line)
           (reduce merge))

      part1 ; drop sand until not settled
      (loop [grid grid i 0]
        (let [[grid settled?] (drop-sand grid)]
          (if settled?
            (recur grid (inc i))
            i)))

      max-y
      (apply max (map second (keys grid)))

      grid ; draw floor for part 2
      (draw-line grid {:x 200 :y (+ 2 max-y)} {:x 1000 :y (+ 2 max-y)})

      part2 ; drop until 500,0 is filled
      (loop [grid grid i 0]
        (let [[grid _] (drop-sand grid)]
          (if (nil? (get grid [500 0]))
            (recur grid (inc i))
            (inc i))))]

  (println part1)
  (println part2))
