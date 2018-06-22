#lang sicp

(define (f-recursive n)
    (cond ((< n 3) n)
          (else(+ (f-recursive (- n 1)) 
                  (* 2 (f-recursive (- n 2)))
                  (* 3 (f-recursive (- n 3))))
          )
    )
)

(define (f-iterative n)
    (define (f a b c n)
      (cond ((< n 0) n)
            ((= n 0) a)
            ((= n 1) b)
            ((= n 2) c) 
            (else (f b c (+ c (* 2 b) (* 3 a)) (- n 1)))
      )
    )
   (f 0 1 2 n)
)


(f-recursive -1)
(f-iterative -1)

(f-recursive 4)
(f-iterative 4)

(f-recursive 30)
(f-iterative 30)