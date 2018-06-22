#lang sicp

;;not working
(define (repeated f n)
    (
        lambda (x)
            (if (= n 1)
                (f x)
                (f ((repeated f (- n 1)) x))
            )
    )
)

 ((repeated (lambda (x) (+ x 1)) 5) 1)