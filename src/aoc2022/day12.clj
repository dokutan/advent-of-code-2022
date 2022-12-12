(ns aoc2022.day12
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]
   [loom.graph :as lg]
   [loom.alg :as lalg]))

(defn move-allowed? [from to]
  (if-not to
    false
    (<= (compare to from) 1)))

(defn allowed-moves [squares row col]
  (let [this (get-in squares [row col])
        left (get-in squares [row (dec col)])
        right (get-in squares [row (inc col)])
        up (get-in squares [(dec row) col])
        down (get-in squares [(inc row) col])

        left (if (move-allowed? this left) [[row (dec col)]] [])
        right (if (move-allowed? this right) [[row (inc col)]] [])
        up (if (move-allowed? this up) [[(dec row) col]] [])
        down (if (move-allowed? this down) [[(inc row) col]] [])]

    {[row col] (concat left right up down)}))

(defn find-square [squares rows cols square]
  (filter
   identity
   (for [row (range rows) col (range cols)]
     (if (= square (get-in squares [row col]))
       [row col]
       nil))))

(println "day 12")
(let [squares (->> "day12"
                   (io/resource)
                   (slurp)
                   (string/split-lines)
                   (map seq)
                   (map vec)
                   (vec))

      rows (count squares)
      cols (count (nth squares 0))

      start (first (find-square squares rows cols \S))
      end (first (find-square squares rows cols \E))

      squares
      (mapv #(mapv (fn [s] (condp = s \S \a \E \z s)) %) squares)

      starts (find-square squares rows cols \a)

      moves
      (apply
       merge
       (for [row (range rows) col (range cols)]
         (allowed-moves squares row col)))

      move-graph (lg/digraph moves)]

  ;; part 1: 472
  (println (dec (count (lalg/bf-path move-graph start end))))

  ;; part 2: 465
  (println (apply min (filter pos? (map #(dec (count (lalg/bf-path move-graph % end))) starts)))))
  