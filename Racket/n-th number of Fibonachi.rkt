#lang sicp
(define (fib n)
  (cond [(= n 0) 0]
        [(= n 1) 1]
        [else (+ (fib (- n 1)) (fib (- n 2))) ]
          )
  )

(define (fib-easy m)
  (define (fib-iter a b count)
    (if (= count 0)
        a
        (fib-iter b (+ a b) (- count 1)
                  )
        )
    )
  (fib-iter 0 1 m)
  )


(define (fib2 n)
  (define (helper n2 n1 i)
          (if (= i n)
              (+ n1 n2)
              (helper n1 (+ n1 n2) (+ i 1))))
  (if (< n 2)
      1
      (helper 1 1 2)))

  (fib2 5) ;; 8