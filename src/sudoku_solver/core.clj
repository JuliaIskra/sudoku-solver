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
  (into [] (replace {(read-string ".") nil} (map read-string (s/split numbers #"")))))


(defn index
  [row col]
  (+ (* 9 row) col))

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


(def digit-set
  (set (range 1 10)))

(def rows
  "Indexes of numbers in each row"
  (into [] (map (partial into []) (partition 9 (range 81)))))

(def cols
  "Indexes of numbers in each column"
  (into [] (map
            (fn [col] (into [] (map
                                (fn [row] (index row col))
                                (range 9))))
            (range 9))))

(def squares
  "Indexes of numbers in each square"
  (into [] (map (partial into [])
                (mapcat
                 (fn [x] (map
                          (fn [y] (square-indexes x y))
                          (range 3)))
                 (range 3)))))

(defn related-row
  "Returns numbers in the same row that indexed number is in"
  [index]
  (let [row-number  (quot index 9)]
    (get rows row-number)))

(defn related-col
  "Returns numbers in the same column that indexed number is in"
  [index]
  (let [col-number (mod index 9)]
    (get cols col-number)))

(defn related-square
  "Returns numbers in the same square that indexed number is in"
  [index]
  (let [row-number (quot index 9)
        col-number (mod index 9)
        square-row (quot row-number 3)
        square-col (quot col-number 3)]
    (get squares (+ (* 3 square-row) square-col))))

(defn get-related-indexes
  [index]
  (concat (related-row index) (related-col index) (related-square index)))

(def related-indexes
  (into [] (map (comp (partial into []) sort distinct get-related-indexes) (range 81))))


(defn possible-numbers
  [field index]
  (let [numbers (transient digit-set)]
    (persistent! (apply disj! numbers (map field (get related-indexes index))))))

(defn first-empty-index
  [field]
  (first (keep-indexed (fn [i n] (if (= n nil) i nil)) field)))

; todo go through each row, col and square and fill only single empty cell in each of them

(defn children
  "Returns all possible correct values in the first empty cell"
  [field]
  (let [index (first-empty-index field)]
    ;(prn "empty index: " index)
    (if (nil? index)
      []
      (do
        ;(prn "rows:     " (related-row field index))
        ;(prn "cols:     " (related-col field index))
        ;(prn "sq:       " (related-square field index))
        ;(prn "possible: " (possible-numbers
        ;                   (related-row field index)
        ;                   (related-col field index)
        ;                   (related-square field index)))
        (map
         (fn [n] (assoc field index n))
         (possible-numbers field index))))))

(defn compute-all
  [field]
  ;(prn "===================================================")
  ;(prn "field:    " field)
  (let [ch (children field)]
    ;(prn "children: " ch)
    (if (and (empty? ch) (not (some nil? field)))
      [field]
      (mapcat compute-all ch))))

(defn compute-first-correct
  [field]
  (let [solutions (compute-all field)]
    (first solutions)))


(defn -main
  [& args]
  (prn (compute-first-correct (to-field (first args)))))
