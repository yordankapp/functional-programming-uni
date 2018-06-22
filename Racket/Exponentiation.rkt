#lang sicp

;;stepenuvane

(define (expt b n)
    (if (= n 0)
        1
        (* b (expt b (- n 1)))))



(define (expt2 b n)
    (expt-iter b n 1))
(define (expt-iter b counter product)
    (if (= counter 0)
        product
        (expt-iter b (- counter 1) (* b product))))



(define (fast-expt b n)
    (cond ((= n 0)1)
          ((even? n)(square (fast-expt b (/ n 2))))
          (else (* b (fast-expt b (- n 1))))))
(define (even? n)
(= (remainder n 2) 0))
(define (square x)
    (* x x))



(expt 2 2) ;; 4
(expt 2 10) ;;1024

(expt2 2 2) ;; 4
(expt2 2 10) ;;1024

(fast-expt 2 2) ;; 4
(fast-expt 2 10) ;;1024