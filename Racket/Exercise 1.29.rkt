#lang sicp

;;Simpson’s Rule
;; ne e okey

(define (sum term a next b)
    (if (> a b)
        0
        (+ (term a) (sum term (next a) next b))))

(define (integral f a b dx)
    (define (add-x dx)
            (+ x dx))
    (* (sum f (+ a (/ dx 2.0)) add-dx b) dx ))

(define (cube x)
    (* x x x))

(define (square x)
    (* x x))

(define (simpsons-method f a b n)
    (define (first-term)
        (/ (- b a) n))
    (define (y-term k)
        (f(+ a (* k (first-term)))))
    (define (skip-two x)
        (+ x 2))
    (* (/ (first-term) 3.0) 
        (+ a
            (* 4 (sum (y-term 1) (skip-two a) (- n 1)))  ;;?????????
            (* 2 (sum (y-term 2) (skip-two a) (- n 1)))  ;; ???????/
            (y-term n)
        )
    )
)

(integral cube 0 1 0.0001)
(integral square 0 1 0.0001)
(simpsons-method cube 0 1 100)
(simpsons-method square 0 1 100)
(simpsons-method cube 0 1 1000)
(simpsons-method square 0 1 1000)
