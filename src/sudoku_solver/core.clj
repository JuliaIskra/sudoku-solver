(ns sudoku-solver.core
  (:gen-class)
  (:require [clojure.string :as s]))

;  c
;r 0  1  2   3  4  5   6  7  8
;  9 10 11  12 13 14  15 16 17
; 18 19 20  21 22 23  24 25 26
;
; 27 28 29  30 31 32  33 34 35
; 36 37 38  39 40 41  42 43 44
; 45 46 47  48 49 50  51 52 53
;
; 54 55 56  57 58 59  60 61 62
; 63 64 65  66 67 68  69 70 71
; 72 73 74  75 76 77  78 79 80

(defn to-field
  [numbers]
  (replace {(read-string ".") nil} (map read-string (s/split numbers #""))))

(defn index
  [row col]
  (+ (* 9 row) col))

(defn rows
  "Returns values in rows of a filed"
  [field]
  (partition 9 field))

(defn cols-indexes
  "Returns indexes of cols"
  []
  (map
   (fn [col] (map
              (fn [row] (index row col))
              (range 9)))
   (range 9)))

(defn cols
  "Returns values in cols of a filed"
  [field]
  (map
   (fn [col-indexes] (map (partial get field) col-indexes))
   (cols-indexes)))

(defn square-indexes
  "Takes square's coordinates and returns indexes of cells in it"
  [x y]
  (let [row (* x 3)
        col (* y 3)]
    (flatten (map
              (fn [r] (map
                       (fn [c] (index r c))
                       (range col (+ col 3))))
              (range row (+ row 3))))))

(defn squares-indexes
  "Returns indexes of squares"
  []
  (mapcat
   (fn [x] (map
            (fn [y] (square-indexes x y))
            (range 3)))
   (range 3)))

(defn squares
  "Returns values in squares of a filed"
  [field]
  (map
   (fn [sq-indexes] (map (partial get field) sq-indexes))
   (squares-indexes)))

(defn unique?
  [input]
  (let [numbers (remove nil? input)]
    (if (empty? numbers)
      true
      (apply distinct? numbers))))

(defn correct?
  [field]
  (and (every? unique? (rows field))
       (every? unique? (cols field))
       (every? unique? (squares field))))

(defn correct-and-without-nil?
  [field]
  (and (correct? field)
       (not (some nil? field))))

(defn first-empty-index
  [field]
  (first (keep-indexed (fn [i n] (if (= n nil) i nil)) field)))

(defn compute-recursive
  [field number empty-index]
  (let [new-empty-index (first-empty-index field)]
    (cond
      (correct-and-without-nil? field) field
      (> number 9) nil
      (and (correct? field) (not= new-empty-index empty-index)) (compute-recursive field 1 new-empty-index)
      :else (compute-recursive (assoc field empty-index number) (inc number) empty-index))))

(defn compute
  [field]
  (compute-recursive field 1 (first-empty-index field)))

(defn -main
  [& args]
  (println "Hello, World!"))
