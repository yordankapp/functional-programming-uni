#lang sicp

(define (square x)
    (* x x))

(define (add a b)
    (+ a b))

(define (prime? n)
    (define (divides a b)
        (= 0 (remainder b a )))
    (define (iter a)
        (cond   ((> (* a a) n) true)
                ((divides a n) false)
                (else (iter (+ a 1)))))
    (iter 2))

(define (filtered-accumulate combiner null-value predicate term a next b)
    (if (> a b)
        null-value
        (if (predicate a b)
            (combiner (term a) (filtered-accumulate combiner null-value predicate term (next a) next b))
            (filtered-accumulate combiner null-value predicate term (next a) next b))))

(define (sum-squares-primes a b)
    (define (is-prime a b)
        (prime? a))
    (filtered-accumulate add 0 is-prime square a inc b))

    (sum-squares-primes 1 5)
