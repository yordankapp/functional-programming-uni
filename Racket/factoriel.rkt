#lang racket
(define (fact n)
  (if (= n 1)
      1
      (* n (fact(- n 1)))
      )
  )

(define (fact2 m)
  (define(fact-iter product counter maxcount)
    (if (> counter maxcount)
        product
        (fact-iter (* product counter)
                   (+ counter 1)
                   maxcount)
        )
    )
  (fact-iter 1 1 m)
  ;;; I LOVE YOU BANANY <3
  )

 (define (fact3 n)
  (letrec ([fact-iter
            (lambda (arg res)
                (if (= arg 0)
                   res
                  (fact-iter
                    (- arg 1) (* arg res))))])
 (fact-iter n 1)))

  