#lang racket 

;; e^x по формулата на Тейлор
(define(fact n)
    (if (= n 0)
        1
        (* n (fact (- n 1)))))

(define (pow x n)
    (if(= n 0 )
        1
        (* x (pow x (- n 1)))))

(define (exp x n)
    (if (= n 0)
        1
        (+ (/ (pow x n) (fact n)) (exp x (- n 1)))))

(define (taylor x n)
    (define (helper i prev sum)
        (if(> i n)
            (+ prev sum)
            (helper (+ i 1) (/ (* prev x) i) (+ sum prev))))
    (helper 1 1 0))

;;дали число А е подниз на Б

(define(include? a b)
    (define (endsWith? a b)
        (cond   ((= a 0) #t)
                ((not (= (remainder a 10) (remainder b 10))) #f)
                (else (endsWith? (quotient a 10) (quotient b 10)))))
    (cond   ( (= b 0) #f)
            ((endsWith? a b) #t)
            (else (include? a (quotient b 10)))))

;; F(b) - F(a)
(define (difference f a b)
    (- (f b) (f a))
)
(define (f1 x)
    (* x 2))

;;(difference f1 2 5);;6


;; Задача 4. Чрез използване на lambda израз да се дефинира процедурен обект,
;; който е еквивалентен на f, ако имаме дефиницията (define (f x) (* 2 x)).

(define (f1_2 x)
    (lambda (x) (* x 2)))

;;(difference (lambda (x) (* x 3)) 2 5) ;;9

;;Задача 5. Да се дефинира процедура от по-висок ред (composition f g), 
;;която намира композицията на едноаргументните реални функции f и g.

(define (composition f g)
    (lambda (x) 
        (f (g x))))

;;((composition (lambda (x) (+ x 1)) f1) 4) ;; 9

;;Задача 6. Да се дефинира процедура от по-висок ред (derive f eps),
;; която намира първа производна на едноаргументната реална функция f с точност eps.
(define (derive f eps)
    (lambda (x) (/ (- (f (+ x eps)) (f x)) eps)))

;;(define f_ (derive (lambda (x) (* x 2)) 1))
;;(f_ 10)  2

;;Задача 7. Да се дефинира процедура от по-висок ред (derive2 f eps), 
;;която намира втора производна на едноаргументната реална функция f с точност eps.

(define (derive2 f eps)
    (derive (derive f eps) eps))

;;((derive (lambda (x) (* 2 x x)) 0.000001 )10)

;;Задача 8. Да се дефинира процедура от по-висок ред (derive-n f n eps), 
;;която намира n-та производна на едноаргументната реална функция f с точност eps.

(define (derive-n f n eps)
    (if (= n 0)
        f
        (derive-n (derive f eps) (- n 1) eps)))
;;((derive-n (lambda (x) (* 2 x x)) 2 0.000001) 10)

;;Задача 9. Да се дефинира процедура от по-висок ред (repeated f n), 
;;която намира n-кратна композиция на едноаргументната реална функция f.
(define (repeated f n)
    (if (= (remainder n 2) 0)
        ()
        (repeated f (- n 1)))