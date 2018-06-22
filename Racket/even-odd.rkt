#lang racket

(define (my-odd? n)
    (letrec ([even?
                 (lambda (x)
                     (or (= x 0) (odd? (- x 1))))]
            [odd?
                (lambda (x)
                    (and (not (= x 0))
                        (even? (- x 1))))])
 (odd? n)))

 (my-odd? 5)
 (my-odd? 8)