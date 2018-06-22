#lang sicp


;;Задача 1 Да се провери дали число е перфектно, т.е равно на сбора на делителите си 
(define (sum-divisors n)
    (define (helper sum i n)
        (if(> i n)
            sum
            (if (= (remainder n i) 0)
                (helper (+ sum i) (+ i 1) n)
                (helper sum (+ i 1) n))))
    (helper 0 1 n))

(define (perfect-number? n)
    (if (= (sum-divisors n) n)
        #t
        #f))

;;(perfect-number? 15);;f

;;Задача 2 Във възходящ ред ли са цифрите в число?

(define (inc-digits? n)
    (define (helper i d)
        (cond  ((= i 0) #t)
               ( (< d (remainder i 10)) #f )
               (else (helper (quotient i 10) (remainder i 10)))))
    (helper n 9))

;;(inc-digits? 50);;f
;;(inc-digits? 112389);;t

;;(lambda (x)
   ;; (+ x 4))

    ;;((lambda (x) (+ x 4)) 3 )
    ;;((lambda (x y) (+ x y)) 1 2)
    ;;((lambda () 7))
   ;; (define (f1 op x y)
    ;;(op x y))

  ;;  (define (mult x y )
   ;; (* x y))

   ;; (define (mult2 x y)
   ;; (f1 (lambda (x y) (* x y)) x y))
 ;;   (mult2 2 5)

;;(next2 3)



;;Задача 3. Да се дефинира функцията (automorphic? n), 
;;която приема число n и проверява дали n^2 завършва с цифрите на n.


(define (automorphic? n)
    (define (helper n n2)
        (if(= n 0) 
                #t
                (if (= (remainder n2 10) (remainder n 10)) 
                        (helper (quotient n 10) (quotient n2 10))
                        #f)))
    (helper n (* n n)))

(automorphic? 25)

;;Задача 4. По зададени x и n, да се изчисли сумата: 1 + x + x^2 + x^3 + ... + x^n
;; (expt x n) -> x^n
(define (searies x n)
    (define (helper res x n)
        (if (= n 0)
            (+ res 1)
            (+ (expt x n) (helper res x (- n 1)))))
    (helper 0 x n))
    
(searies 2 1)