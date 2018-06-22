#lang sicp

(define (pi-sum a b)
    (if (> a b)
        0
        (+ (/ 1.0 (* a (+ a 2)))
            (pi-sum (+ a 4) b))))
(pi-sum 1 7)


(define (pi-sum2 a b)
    (define (pi-term x)
        (/ 1.0 (* x (+ x 2))))
    (define (pi-next x)
        (+ x 4))
    (sum pi-term a pi-next b))
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a) 
         (sum term (next a) next b))))

(pi-sum2 1 7)


(define (pi-sum-lambda a b)
 (sum (lambda (x) (/ 1 (* x (+ x 2))))
 a
 (lambda (x) (+ x 4))
 b))

;;(pi-sum-lambda 1 7)
