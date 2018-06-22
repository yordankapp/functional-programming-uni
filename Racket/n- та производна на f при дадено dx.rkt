#lang sicp

(define (derive f dx)
    (lambda (x) (/ (- (f (+ x dx) (f x)) dx))))

(define (Df f n dx)
    (if(= n 1)
        (derive f dx)
        (Df (derive f dx) (- n 1) dx)))

