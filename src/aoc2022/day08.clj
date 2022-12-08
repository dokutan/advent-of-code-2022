(ns aoc2022.day08
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]))

(defn look-left [trees [row col]]
  (reverse (take col (nth trees row))))

(defn look-right [trees [row col]]
  (take-last (- (count (nth trees row)) col 1) (nth trees row)))

(defn look-up [trees [row col]]
  (reverse
   (map
    #(nth % col)
    (take row trees))))

(defn look-down [trees [row col]]
  (map
   #(nth % col)
   (take-last (- (count trees) row 1) trees)))

(defn tree-visible? [tree tree-line]
  (let [highest-tree (if (empty? tree-line)
                       -1
                       (apply max tree-line))]
    (> tree highest-tree)))

(defn count-visible-trees [tree tree-line]
  (let [result (count (take-while #(< % tree) tree-line))]
    (if (< result (count tree-line))
      (inc result)
      result)))

(println "day 08")
(let [trees (->> "day08"
                 (io/resource)
                 (slurp)
                 (string/split-lines)
                 (map seq)
                 (map #(map (fn [c] (Integer/parseInt (str c))) %))
                 (map vec)
                 (vec))

      rows (count trees)
      cols (count (nth trees 0))]

  ;; part 1: 1851
  (println
   (count
    (filter
     identity
     (for [row (range rows)
           col (range cols)]

       (let [tree (get-in trees [row col])
             left (look-left trees [row col])
             right (look-right trees [row col])
             up (look-up trees [row col])
             down (look-down trees [row col])]

         (or
          (tree-visible? tree left)
          (tree-visible? tree right)
          (tree-visible? tree up)
          (tree-visible? tree down)))))))

  ;; part 2: 574080
  (println
   (apply
    max
    (for [row (range rows)
          col (range cols)]

      (let [tree (get-in trees [row col])
            left (look-left trees [row col])
            right (look-right trees [row col])
            up (look-up trees [row col])
            down (look-down trees [row col])]

        (*
         (count-visible-trees tree left)
         (count-visible-trees tree right)
         (count-visible-trees tree up)
         (count-visible-trees tree down)))))))