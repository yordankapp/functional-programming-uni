#lang racket 

(define (square x) (* x x))

(define (f x y)
    (define (helper a b)
        (+ (* x (square a))
            (* y b)
            (* a b)))
    (helper (+ 1 (* x y)) (- 1 y))
)

;;(f 1 2) 4

(define (f-lambda x y)
   ((lambda (a b)
        (+ (* x (square a))
            (* y b)
            (* a b)))

    (+ 1 (* x y))
    (- 1 y))
)

;;(f-lambda 1 2)

(define (f-let x y)
    (let (( a (+ 1 (* x y)))
         ( b (- 1 y)) )

         (+ (* x (square a))
            (* y b)
            (* a b))
    )
)
;;(f-let 1 2)