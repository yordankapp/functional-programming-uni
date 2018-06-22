#lang sicp

;; The idea of the algorithm is based on the observation that, if r is the
;;remainder when a is divided by b, then the common divisors of a and b
;;are precisely the same as the common divisors of b and r . us, we can use the 
;;equation GCD(a,b) = GCD(b,r)


(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b ))))

(gcd 206 40) ;; 2

;;
(define (user-gcd u v)
  (define (pos-gcd u v)
    (if (> u v)
        (bas-gcd u v)
        (bas-gcd v u)
        )
    )
  (define (bas-gcd u v)
    (if (= v 0)
        u
        (bas-gcd v (remainder u v))
        )
    )
  (cond [ (= u 0) (abs v)]
         [ (= v 0) (abs u)]
          [else (pos-gcd (abs u) (abs v))]
          )
  )


