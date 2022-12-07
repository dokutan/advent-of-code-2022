(ns aoc2022.day07
  (:require
   [clojure.java.io :as io]
   [instaparse.core :as insta]))

(def fsecond (comp first second))
(def ssecond (comp second second))
(def rrest (comp reverse rest reverse))

(def cd-ls-parser
  (insta/parser
   "commands = *command
    command = (cd / ls)
    cd = <'$ cd '> (*VCHAR / '/') <LF>
    ls = <'$ ls'> <LF> *(dir / file)
    dir = <'dir '> name <LF>
    file = size <' '> name <LF>
    size = *DIGIT
    name = *VCHAR"
   :output-format :hiccup
   :input-format :abnf))

(defn cd-ls-transform [tree]
  (insta/transform
   {:DIGIT identity
    :VCHAR identity
    :size (comp read-string str)
    :name str
    :cd (fn [& a] [:cd (apply str a)])
    :file (fn [size  name] {:size size :name name})}
   tree))

(defn commands->file-tree+dirs [commands]
  (loop [commands commands cwd [] file-tree {} dirs []]
    (let [command (first commands)
          commands (rest commands)

          cwd
          (if (= :cd (fsecond command))
            (cond
              (= ".." (ssecond command))
              (vec (rrest cwd))
              (= "/" (ssecond command))
              ["/"]
              :else (conj cwd (ssecond command)))
            cwd)

              ;; add dir
          file-tree (if-not (get-in file-tree cwd)
                      (assoc-in file-tree cwd {})
                      file-tree)

              ;; add files
          file-tree (if (= :ls (fsecond command))
                      (assoc-in file-tree cwd {:files (filter map? (rest (first (rest command))))})
                      file-tree)]

      (if (zero? (count commands))
        [file-tree dirs]
        (recur commands cwd file-tree (conj dirs cwd))))))

(defn calc-dir-sizes [file-tree dirs]
  (loop [dirs dirs dir-sizes {} file-tree file-tree]
    (let [dir (first dirs)
          dirs (rest dirs)

          size-files
          (reduce
           +
           (map
            #(:size %)
            (:files (get-in file-tree dir))))

          size-subdirs
          (reduce
           +
           (map
            #(get % :size 0)
            (filter map?
                    (vals
                     (get-in file-tree dir)))))

          size (+ size-files size-subdirs)

          file-tree
          (assoc-in
           file-tree
           dir
           (assoc
            (get-in file-tree dir)
            :size size))

          dir-sizes (assoc dir-sizes dir size)]

      (if-not (zero? (count dirs))
        (recur dirs dir-sizes file-tree)
        dir-sizes))))

(println "day 07")
(let [input (->> "day07"
                 (io/resource)
                 (slurp))

      commands (->> input
                    cd-ls-parser
                    cd-ls-transform
                    rest)

      [file-tree dirs] (commands->file-tree+dirs commands)
      dirs (distinct (sort #(> (count %1) (count %2)) dirs))
      dir-sizes (calc-dir-sizes file-tree dirs)

      required-space (- 30000000 (- 70000000 (get dir-sizes ["/"])))]

  ;; part 1: 1232307
  (println (reduce + (filter #(<= % 100000) (vals dir-sizes))))

  ;; part 2: 7268994
  (println (->> dir-sizes
                (vals)
                (filter #(>= % required-space))
                (sort)
                (first))))
