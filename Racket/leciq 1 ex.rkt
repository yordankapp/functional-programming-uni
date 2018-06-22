#lang sicp

;; suma ot i=1 do n na i / i*i +1

(define (square x)
    (* x x))

(define (sum n)
    (if (= n 0)
        0
        (+ (/ n (+ (square n) 1)) (sum (- n 1)))
    ))

(sum 3) ;;6/5