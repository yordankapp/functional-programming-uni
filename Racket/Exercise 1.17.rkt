#lang sicp

(define (* a b)
    (if (= b 0)
        0
        (+ a (* a (- b 1)))))

(* 2 3)



(define (even? n)
    (= (/ n 2) 0))

(define (double n)
    (* 2 n))

(define (halve n)
    (/ n 2))

(define (multiply a b)
    (cond ((< a 0) (multiply (- a) (- b)))
         ((or (= a 0) (= b 0)) 0) 
         ((= a 1) b)
         ((even? a) (multiply (halve a) (double b)))
         (else (+ b (multiply (- a 1) b))) ))

(multiply 2 3)
(multiply 1000 -100000)
(multiply -1000 100000)