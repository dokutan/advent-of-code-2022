(ns aoc2022.day09
  (:require [clojure.string :as string])
  (:require [clojure.java.io :as io]))

(defn parse-move [move]
  (let [[direction distance] (string/split move #" ")
        distance (Integer/parseInt distance)]
    {:direction direction
     :distance distance}))

(defn do-move [pos direction distance]
  (condp = direction
    "L" [(- (first pos) distance) (second pos)]
    "R" [(+ (first pos) distance) (second pos)]
    "U" [(first pos) (+ (second pos) distance)]
    "D" [(first pos) (- (second pos) distance)]))

(defn adjacent? [pos1 pos2]
  (let [x-difference (abs (- (first pos1) (first pos2)))
        y-difference (abs (- (second pos1) (second pos2)))]
    (and
     (<= x-difference 1)
     (<= y-difference 1))))

(defn move-tail [head-pos tail-pos]
  (if (adjacent? head-pos tail-pos)
    tail-pos
    [(cond
       (> (first head-pos) (first tail-pos))
       (inc (first tail-pos))

       (< (first head-pos) (first tail-pos))
       (dec (first tail-pos))

       :else (first tail-pos))
     (cond
       (> (second head-pos) (second tail-pos))
       (inc (second tail-pos))

       (< (second head-pos) (second tail-pos))
       (dec (second tail-pos))

       :else (second tail-pos))]))

(println "day 09")
(let [moves (->> "day09"
                 (io/resource)
                 (slurp)
                 (string/split-lines)
                 (map parse-move))
      moves (flatten
             (map
              #(repeat (:distance %) {:direction (:direction %) :distance 1})
              moves))
      tail-trail (loop [moves moves
                        head-pos [0 0]
                        tail-pos head-pos
                        tail-trail [tail-pos]]

                   (let [move (first moves)
                         moves (next moves)

                         direction (:direction move)
                         distance 1 ;(:distance move)

                         head-pos (do-move head-pos direction distance)
                         tail-pos (move-tail head-pos tail-pos)

                         tail-trail (conj tail-trail tail-pos)]

                     (if-not moves
                       tail-trail
                       (recur moves head-pos tail-pos tail-trail))))
      tail-trail-2 (loop [moves moves
                          head-pos [0 0]
                          pos-1 head-pos
                          pos-2 head-pos
                          pos-3 head-pos
                          pos-4 head-pos
                          pos-5 head-pos
                          pos-6 head-pos
                          pos-7 head-pos
                          pos-8 head-pos
                          pos-9 head-pos
                          tail-trail [pos-9]]

                     (let [move (first moves)
                           moves (next moves)

                           direction (:direction move)
                           distance 1 ;(:distance move)

                           head-pos (do-move head-pos direction distance)
                           pos-1 (move-tail head-pos pos-1)
                           pos-2 (move-tail pos-1 pos-2)
                           pos-3 (move-tail pos-2 pos-3)
                           pos-4 (move-tail pos-3 pos-4)
                           pos-5 (move-tail pos-4 pos-5)
                           pos-6 (move-tail pos-5 pos-6)
                           pos-7 (move-tail pos-6 pos-7)
                           pos-8 (move-tail pos-7 pos-8)
                           pos-9 (move-tail pos-8 pos-9)

                           tail-trail (conj tail-trail pos-9)]

                       (if-not moves
                         tail-trail
                         (recur
                          moves
                          head-pos
                          pos-1
                          pos-2
                          pos-3
                          pos-4
                          pos-5
                          pos-6
                          pos-7
                          pos-8
                          pos-9
                          tail-trail))))]

  ;; part 1: 6067
  (println (count (distinct tail-trail)))
  
  ;; part 2: 2471
  (println (count (distinct tail-trail-2))))
