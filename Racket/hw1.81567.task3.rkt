#lang racket

(define (encode xs)
    (define (convert xs)
        (if(null? xs)
            null
            (cons  (cons (car xs) (find-count xs)) 
                   (convert (repeat xs (find-count xs))))))
    (convert xs))

  (define (find-count xs)
        (define (helper xs counter)
            (cond   ((or (null? xs) (null? (cdr xs))) counter)
                    ((eqv? (car xs) (car (cdr xs))) (helper (cdr xs) (+ counter 1)))
                    (else (helper null counter))))
    (helper xs 1))

 (define (repeat xs n)
        (define (helper xs i n)
            (if(= i n)
                xs
                (helper (cdr xs) (+ i 1) n)))
        (helper xs 0 n))
