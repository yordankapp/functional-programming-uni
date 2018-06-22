#lang racket
(define (cube x)
  (* x x)
  )
(define (sum a b)
  (if (> a b)
      0
      (+ (cube a) (sum (+ a 1 ) b))
    )
  )
  