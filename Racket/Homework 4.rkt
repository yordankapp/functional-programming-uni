#lang racket

(define (maximize fs) 
    (lambda(x) (argmax abs (map (lambda(f) (f x)) fs))))

