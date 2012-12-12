(ns supersudoku.core 
  (:refer-clojure :exclude (==))
  (:use clojure.core.logic))
 
(use 'clojure.pprint)

(defn rows [table width]
  (partition width table))

(defn cols [table width]
  (apply map list (rows table width)))

(defn boxs [table width]
  (let [boxsz (int (Math/sqrt width))]
    (->> (for [i (range boxsz)]
           (partition boxsz 
                      (partition boxsz width 
                                 (drop (* i boxsz) table))))
         flatten
         (partition width))))

(defn solve-sudoku [sudoku]
  (let [width (int (Math/sqrt (count sudoku)))
        boxsz (int (Math/sqrt width))
        table (map (fn [x] (if (zero? x) (lvar) x)) sudoku)
        r (rows table width)
        c (cols table width)
        b (boxs table width)
        d (repeatedly (* width width) lvar)] 

    (first 
     (run 1 [q]
          
          (everyg (fn [x] 
                    (infd x 
                          (apply domain (range 1 (+ 1 width))))) table)

          (== d table)
          (== q table)

          (everyg distinctfd r)
          (everyg distinctfd c)
          (everyg distinctfd b)))))

(def tt 
  [0 0  4 0
   1 0  0 0

   0 0  0 3
   0 1  0 0])

(def tt2
  [8 0 0  0 0 0  0 0 0
   0 0 3  6 0 0  0 0 0
   0 7 0  0 9 0  2 0 0

   0 5 0  0 0 7  0 0 0
   0 0 0  0 4 5  7 0 0
   0 0 0  1 0 0  0 3 0

   0 0 1  0 0 0  0 6 8
   0 0 8  5 0 0  0 1 0
   0 9 0  0 0 0  4 0 0])

(defn -main []
  (pprint (partition 4 (solve-sudoku tt))))
