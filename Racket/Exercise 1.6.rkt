#lang sicp

(define (new-if predicate then-clause else-clause)
    (cond (predicate then-clause)
    (else else-clause)))
;; lexical scoping
(define (sqrt x)
    (define (good-enough? guess)
        (< (abs (- (square guess) x)) 0.001))
    (define (improve guess)
            (average guess (/ x guess)))
    (define (sqrt-iter guess)
        (if (good-enough? guess)
            guess
            (sqrt-iter (improve guess))))
    (sqrt-iter 1.0))

(sqrt 9)