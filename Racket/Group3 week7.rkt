#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Помощни макроси за "pretty-print" на примерите ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax-rule (example stx)
  (if (string? stx)
      (printf "; ~a~n" stx)
      (printf "> ~s → ~v~n" 'stx stx)))

(define-syntax examples
  (syntax-rules ()
    [(examples x)
     (begin (example x) (displayln ""))]
    [(examples x y ...)
     (begin (example x) (examples y ...))]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Полезни помощни функции върху числа
(define (last-digit n) (remainder n 10))
(define (front-digits n) (quotient n 10))
(define (divides? k n) (= 0 (remainder n k)))

; Основни функции върху списъци:
; - cons, car, cdr
; - null, null?
; - list, list?
; - map, filter, apply
; - length, reverse, member, remove-duplicates

; Полезни помощни функции върху списъци
(define (minimum xs) (apply min xs))
(define (maximum xs) (apply max xs))
(define (sum xs) (apply + xs))
(define (product xs) (apply * xs))
(define (concat xss) (apply append xss))


; Задача 1. Дефинирайте функцията (max-prime n), която връща
; най-голямото просто число по-малко или равно на n.
(define (max-prime n) 
    (define (find-prime n i)
        (if (prime? i)
            i
            (find-prime n (- i 1))))
    (find-prime n n))

(define (prime? n)
    (define (helper n i)
        (cond   ((= n i) #t)
                ((not(=(remainder n i)0)) (helper n (+ i 1)))
                (else #f)))
    (helper n 2))

; (max-prime 10)
; (max-prime 100)
 
; Задача 2. Дефинирайте функцията (inside-cirlce? c r), която
; приема n-мерна точка c (представена като списък от компоненти)
; и число r, и връща функция, чиято стойност в дадена точка p
; е дали p се намира вътре в n-мерната сфера с център c и радиус r.
(define (inside-circle? c r) 
    (lambda (p) (<= (distance c p) r)))

(define (distance xs ys)
    (sqrt(sum(map(lambda (x) (* x x)) (map - xs ys)))))


;;;  ((inside-circle? '(0 0) 1) '(0 1))
;;;  ((inside-circle? '(0 0) 1) '(1 0))
;;;  ((inside-circle? '(0 0) 1) '(1 1))
 


; Задача 3. Дефинирайте функцията (count-min-crosses fs), която
; приема списък от числа fs, съответсващи на стойностита на дадена
; непрекъсната функция f в интервала [0 .. n] и връща минималния
; брой пъти, които f пресича абсцисата в дадения интервал.
(define (count-min-crosses fs) 
    (cond   ((null? fs) 0)
            ((= 0 (car fs)) (+ 1 (count-min-crosses (cdr fs))))
            ((null? (cdr fs)) 0)
            ((< (* (car fs) (cadr fs))0) (+ 1 (count-min-crosses (cdr fs))))
            (else (count-min-crosses (cdr fs)))))

;;; (examples
;;;  (count-min-crosses '(0 1 2 3 4))
;;;  (count-min-crosses '(-1 1 -1 1))
;;;  )

; Задача 4. Дефинирайте функцията (reverse-column i xss),
; която приема матрица xss (представена като списъс от списъци)
; и индекс на колона i (започващ от нула) и обръща елементите
; и връща матрица, в която елементите на i-тата колона са
; обърнати.
; Примери:
;
; (reverse-column 0 '((1 2 3)    → '((7 2 3)
;                     (4 5 6)        (4 5 6)
;                     (7 8 9)))      (1 8 9))
;
; (reverse-column 1 '((1 2 3)    → '((1 8 3)
;                     (4 5 6)        (4 5 6)
;                     (7 8 9)))      (7 2 9))


;;; (define (get-element i xs)
;;;     (if (= i 0)
;;;         (car xs)
;;;         (get-element (- i 1) (cdr xs))))

;;; (define (get-column i xss)
;;;     (map (lambda (row) (get-element i row)) xss))

;;; (define (set-element i xs val)
;;;     (if (= i 0) 
;;;         (cons val (cdr xs))
;;;         (cons (car xs) (set-element (- i 1) (cdr xs) val))))

;;; ;;; (define(set-column i xss ys)
;;; ;;;     (map (lambda (row val) (set-element i row val)) matrix col))

;;; (define (reverse-column i matrix)
;;;     (set-column i matrix (reverse (get-column i matrix))))

;;; (examples
;;;  (reverse-column 0 '((1 2 3)
;;;                      (4 5 6)
;;;                      (7 8 9)))
;;;  (reverse-column 1 '((1 2 3)
;;;                      (4 5 6)
;;;                      (7 8 9)))
;;;  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Допълнителни задачи ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Задaча 1. Да се дефинира функцията (fib-sum-even n), която връща сбора
; на четните числа на фибоначи, които са по-малки или равни на n.
(define (fib-sum-even n) 
    
    (define (helper n i sum)
        (cond   ((> (find-i-fib i) n) sum)
                ((even? (find-i-fib i)) (helper n (+ i 1) (+ sum (find-i-fib i))))
                (else (helper n (+ i 1) sum))))
    (helper n 1 0)
)

(define (find-i-fib i)
        (cond [(= i 0) 0]
              [(= i 1) 1]
              [else (+ (find-i-fib (- i 1)) (find-i-fib (- i 2))) ]))

 ;(fib-sum-even 7)
; (fib-sum-even 1000)
 

; Задача 2. Да се дефинира функцията (list-palindromes a b), която връща
; списък с всички числа палиндроми в интервала [a .. b].
(define (list-palindromes a b) 
     (filter (lambda (x) (= (reverse x) x)) (range a (+ b 1))))

(define (reverse x)
    (define (helper x rev)
        (if (= x 0)
            rev
            (helper (quotient x 10) (+ (* rev 10) (remainder x 10)))))
    (helper x 0))


 ;(list-palindromes 10 99)
 ;(list-palindromes 300 500)

; Задача 3. Дефинирайте функцията (closest-outside-circle c r ps),
; която приема n-мерна точка c, число r и списък от n-мерни точки ps
; и връща най-близката до c точка от ps, която се намира извън кръга
; с център c и радиус r.
(define (closest-outside-circle c r ps) #f)

(examples
 (closest-outside-circle '(0 0) 1 '((0 1) (2 2) (1 0) (1 1)))
 (closest-outside-circle '(1 1) 1 '((0 1) (2 2) (1 0) (1 1)))
 )

; Задача 4. Дефинирайте функцията (diagonal-product matrix),
; която приема числова квадратна матрица matrix и връща
; скаларното произведение на двата и диагонала.

; вече решена
