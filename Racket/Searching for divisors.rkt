#lang sicp

(define (smallest_divisor n)
    (find_divisor n 2))

(define (find_divisor n test_divisor)
    (cond   ((> (square test_divisor) n) n)
            ((divides? test_divisor n) test_divisor)
            (else (find_divisor n (+ test_divisor 1)))
    )
)

(define (square x)
    (* x x))
(define (divides? a b)
    (= (remainder b a) 0))

(smallest_divisor 15);;3