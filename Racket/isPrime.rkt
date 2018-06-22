#lang sicp

(define (smallest_divisor n)
    (find_divisor n 2))

(define (find_divisor n test_divisor)
    (cond   ((> (square test_divisor) n) n)
            ((divides? test_divisor n) test_divisor)
            (else (find_divisor n (+ test_divisor 1)))
    )
)

(define (square x)
    (* x x))
(define (divides? a b)
    (= (remainder b a) 0))

(define (prime? n)
    (= n (smallest_divisor n)))

(prime? 11) ;;#t
(prime? 6) ;;#f

(define (prime2? n) ;; not ok 
    (define (helper a n)
        (cond   ((> a (sqrt n)) #t )
                ( (= (remainder n a) 0) #f)
                ( else(helper (+ a 1) n))))
   ( if(= a 1)
        #f
        (helper 2 n))
)

(prime2? 1)