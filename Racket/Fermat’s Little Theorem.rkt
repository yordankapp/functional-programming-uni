#lang sicp

(define (expmod base exp n)
    (  cond ((= exp 0) 1)
            ((even? exp) (remainder (square (expmod base (/ exp 2) n))))
            (else (remainder (* base (expmod base (- exp 1) n )) n))
    ))

(define (fermat_test n)
    (define (try_it a)
        (= (expmod a n n) a))
    (try_it (+ 1 (random (- n 1)))))

(define (fast_prime? n times)
    (cond   ((= times 0) 0)
            ((fermat_test n ) (fast_prime? n (- times 1)))
            (else false)
     ))

(define (square x)
    (* x x))

(define (even? n)
    (= (/ n 2) 0))
