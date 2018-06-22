#lang sicp

(define (pascal-triangle row col)
        (cond ((or (< row 1) (< col 0) (> col row)) 0)
              ((and (= col 1) (= row 1)) 1)
              (else (+ (pascal-triangle (- row 1) (- col 1))
                         (pascal-triangle (- row 1) col)))))

(pascal-triangle 1 2)
(pascal-triangle 5 10)

(pascal-triangle 5 1)
(pascal-triangle 5 2)
(pascal-triangle 5 3)
(pascal-triangle 5 4)
(pascal-triangle 5 5)

(pascal-triangle 25 12)