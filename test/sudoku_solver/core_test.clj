(ns sudoku-solver.core-test
  (:require [clojure.test :refer :all]
            [sudoku-solver.core :refer :all]))

(def n nil)

(deftest solve-sudoku-with-one-unknown-cell
  (is (= (compute-first-correct [1 7 4 3 8 5 9 6 2
                                 2 9 3 4 6 7 1 5 8
                                 5 8 6 1 9 2 7 3 4
                                 4 5 1 9 2 3 8 7 6
                                 9 2 8 6 7 4 3 1 5
                                 3 6 7 8 5 1 2 4 9
                                 7 1 9 5 4 8 6 2 3
                                 6 3 5 2 1 9 4 8 7
                                 8 4 2 7 3 6 n 9 1])
         [1 7 4 3 8 5 9 6 2
          2 9 3 4 6 7 1 5 8
          5 8 6 1 9 2 7 3 4
          4 5 1 9 2 3 8 7 6
          9 2 8 6 7 4 3 1 5
          3 6 7 8 5 1 2 4 9
          7 1 9 5 4 8 6 2 3
          6 3 5 2 1 9 4 8 7
          8 4 2 7 3 6 5 9 1])))

(deftest solve-sudoku-with-two-unknown-cells
  (is (= (compute-first-correct [1 7 4 3 8 5 9 6 2
                                 2 9 3 4 6 7 1 5 8
                                 5 8 6 1 9 2 7 3 4
                                 4 5 1 9 2 3 8 7 6
                                 9 2 8 6 7 4 3 1 5
                                 3 6 7 8 5 1 2 4 9
                                 7 1 9 5 4 8 6 2 3
                                 6 3 n 2 1 9 4 8 7
                                 8 4 2 7 3 6 n 9 1])
         [1 7 4 3 8 5 9 6 2
          2 9 3 4 6 7 1 5 8
          5 8 6 1 9 2 7 3 4
          4 5 1 9 2 3 8 7 6
          9 2 8 6 7 4 3 1 5
          3 6 7 8 5 1 2 4 9
          7 1 9 5 4 8 6 2 3
          6 3 5 2 1 9 4 8 7
          8 4 2 7 3 6 5 9 1])))

(deftest solve-sudoku-with-more-than-one-possible-number
  (is (= (compute-first-correct [1 n 4 3 8 5 9 6 n
                                 n 9 3 4 6 7 1 5 8
                                 5 8 6 1 9 2 7 3 4
                                 4 5 1 9 2 3 8 7 6
                                 9 n 8 6 7 4 3 1 5
                                 3 6 7 8 5 1 2 4 9
                                 7 1 9 5 4 8 6 2 3
                                 6 3 5 2 1 9 4 8 7
                                 8 4 2 7 3 6 n 9 1])
         [1 7 4 3 8 5 9 6 2
          2 9 3 4 6 7 1 5 8
          5 8 6 1 9 2 7 3 4
          4 5 1 9 2 3 8 7 6
          9 2 8 6 7 4 3 1 5
          3 6 7 8 5 1 2 4 9
          7 1 9 5 4 8 6 2 3
          6 3 5 2 1 9 4 8 7
          8 4 2 7 3 6 5 9 1])))

; 23s -> 8s
(deftest solve-hard-sudoku
  (is (= (time (compute-first-correct [1 n n n n n n n 2
                                       n 9 n 4 n n n 5 n
                                       n n 6 n n n 7 n n
                                       n 5 n 9 n 3 n n n
                                       n n n n 7 n n n n
                                       n n n 8 5 n n 4 n
                                       7 n n n n n 6 n n
                                       n 3 n n n 9 n 8 n
                                       n n 2 n n n n n 1]))
         [1 7 4 3 8 5 9 6 2
          2 9 3 4 6 7 1 5 8
          5 8 6 1 9 2 7 3 4
          4 5 1 9 2 3 8 7 6
          9 2 8 6 7 4 3 1 5
          3 6 7 8 5 1 2 4 9
          7 1 9 5 4 8 6 2 3
          6 3 5 2 1 9 4 8 7
          8 4 2 7 3 6 5 9 1])))

; 0.8s -> 0.3s
(deftest solve-hard-sudoku2
  (is (= (time (compute-first-correct [n 8 n 6 n n 3 n n
                                       n n n n n 3 n 8 5
                                       3 n 1 n 2 n n n n
                                       n n n 5 n 7 n n 4
                                       6 n 4 n n n 1 n n
                                       n 9 n n 6 n n n 2
                                       5 n n 2 n n n 7 3
                                       n n 2 n 1 n n n n
                                       n n n n 4 n n n 9]))
         [9 8 7 6 5 4 3 2 1
          2 4 6 1 7 3 9 8 5
          3 5 1 9 2 8 7 4 6
          1 2 8 5 3 7 6 9 4
          6 3 4 8 9 2 1 5 7
          7 9 5 4 6 1 8 3 2
          5 1 9 2 8 6 4 7 3
          4 7 2 3 1 9 5 6 8
          8 6 3 7 4 5 2 1 9])))

;(deftest solve-hardest-sudoku
;  (is (= (time (compute-first-correct [n n n n n n n n n
;                                       n n n n n 3 n 8 5
;                                       n n 1 n 2 n n n n
;                                       n n n 5 n 7 n n n
;                                       n n 4 n n n 1 n n
;                                       n 9 n n n n n n n
;                                       5 n n n n n n 7 3
;                                       n n 2 n 1 n n n n
;                                       n n n n 4 n n n 9]))
;         [9 8 7 6 5 4 3 2 1
;          2 4 6 1 7 3 9 8 5
;          3 5 1 9 2 8 7 4 6
;          1 2 8 5 3 7 6 9 4
;          6 3 4 8 9 2 1 5 7
;          7 9 5 4 6 1 8 3 2
;          5 1 9 2 8 6 4 7 3
;          4 7 2 3 1 9 5 6 8
;          8 6 3 7 4 5 2 1 9])))
