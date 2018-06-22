#lang racket
(+ 5 3)
(define x 3)
x
(* 5 x)
(* (+ 2 1) (- 3 1))

(define (square x)(* x x))

(define (absol x)
  (cond [(> x 0) x]
        [(= x 0) 0]
        [(< x 0) (- x)]
   )
  )

(define (abs x)
  (if(< x 0) (- x) x)
  )
