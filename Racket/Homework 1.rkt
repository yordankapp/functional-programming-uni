#lang racket

 (define (convert x k n)
     (decimal-to-n (k-to-decimal x k) n))

(define (k-to-decimal x k)
    (define (helper decimalnumber i x k)
        (if (= x 0)
            decimalnumber
            (helper (+ decimalnumber (* (remainder x 10) (pow k i))) (+ i 1) (quotient x 10) k)))
    (helper 0 0 x k))

(define (decimal-to-n x n)
    (define (helper result i x n)
        (if(= x 0)
            result
            (helper (+ result (* (remainder x n) i)) (* i 10) (quotient x n) n)))
    (helper 0 1 x n))

(define (pow k i)
    (if(= i 0)
        1
        (* k (pow k (- i 1)))))
