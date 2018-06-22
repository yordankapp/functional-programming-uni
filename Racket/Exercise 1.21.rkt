#lang sicp

(define (smallest_divisor n)
    (define (find_next_div n div)
        (if (<= (square div) n)
            (if (= (remainder n div) 0)
                div
                (find_next_div n (+ div 1)))
        n)
    )
    (find_next_div n 2)
)

(define (square n)
    (* n n))

(smallest_divisor 199)