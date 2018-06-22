#lang sicp

(define (even? n)
    (= (/ n 2) 0))

(define (double n)
    (* 2 n))

(define (halve n)
    (/ n 2))


(define (multiply-iter a b)
    (define (helper a b c)
        (cond ((= a 0) c)
              ((< a 0) (helper (- a) (- b) c))
              (else
                (if (even? a)
                    (helper (halve a) (double b) c)
                    (helper (- a 1) b (+ c b))
                ) 
              )
        )
    ) 
    (helper a b 0))

(multiply-iter 2 3)
(multiply-iter 1000 -100000)
(multiply-iter -1000 100000)