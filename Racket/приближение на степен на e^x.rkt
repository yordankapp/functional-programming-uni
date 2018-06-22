#lang sicp

;; e^x= 1 + x/1! + x^2/2! + .... +x^n/n!


;;recursive:
(define (E_number x n)
    (if (= n 0)
        1
        (+  (/ (pow x n) (fact n))
            (E_number x (- n 1))) ))

(define (fact x)
    (if (= x 0)
        1
        (* x (fact(- x 1)))))

(define (pow x n)
    (if(= n 0)
        1
        (* x (pow x (- n 1)))))

(E_number 1 3)


;;iterative : not working well

(define (E_number_iter x n)
    (helper x n 1))

(define (helper x n curr)
    (if (= n -1)
        curr
        (helper 
                x
                (- n 1)
                (+ (* curr (/ x (+ n 1)))  1) 
        )
    )
)

(E_number_iter 1 3)