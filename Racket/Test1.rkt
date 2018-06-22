#lang racket

;;; Задача 1. Да се напише функция (sum-numbers a b), приемаща два аргумента, която
;;; намира сумата на числата в интервала [a,b], чиито цифри са в низходящ (>=) ред

(define (sum-numbers a b)
    (apply + (filter (lambda (x) (descending-order x)) (range a (+ b 1))))   
)

(define (descending-order x)
    (define (helper x y)
        (cond   ((and (< x 10) (= y 0)) #t)
                ((<= (remainder x 10) (remainder y 10)) (helper (quotient x 10) (quotient y 10)))
                (else #f )))
    (helper x (quotient x 10)))


;(sum-numbers 1 9); → 45
;(sum-numbers 199 203) ;→ 200
;(sum-numbers 219 225); → 663

;; Зад 2 вече решавана

;;; Задача 3. Ако f и g са числови функции и n е естествено число, да се напише функция от по-
;;; висок ред (switchsum f g n), която връща като резултат функция, чиято стойност в дадена
;;; точка x е равна на f(x)+g(f(x))+f(g(f(x)))+ ... (сумата включва n събираеми).

(define (switchsum f g n)
    (define (helper f g n sum x)
        (cond   ((= n 0) sum)
                (else (helper f g (- n 1) (+ sum (repeat-compose f g n x)) x))))
    (lambda (x) (helper f g n 0 x)))

(define (repeat-compose f g n x)
    (define (helper f g n i result)
        (cond   ((= n i) result)
                ((= (remainder i 2) 0) (helper f g n (+ i 1) (f result)))
                (else (helper f g n (+ i 1) (g result)))))
    (helper f g n 0 x))

;;;  ((switchsum (lambda (x) (+ x 1)) (lambda (x) (* x 2)) 1) 2); → 3
;;; ((switchsum (lambda (x) (+ x 1))
;;;  (lambda (x) (* x 2)) 2) 2) ;→ 9
;;; ((switchsum (lambda (x) (+ x 1))
;;;  (lambda (x) (* x 2)) 3) 2) ;→ 16
;;; ((switchsum (lambda (x) (+ x 1))
;;;  (lambda (x) (* x 2)) 4) 2) ;→ 30


;;; Задача 4. Напишете функция (repeater str), която получава като аргумент символен низ и
;;; връща анонимна функция на два аргумента - count и glue (число и низ). Оценката на
;;; обръщението към върнатата функция е низ, който се получава чрез count-кратно повтаряне
;;; на низа str, при което между всеки две съседни повторения на str стои низът glue.

(define(repeater string)
    (lambda (count glue) (repeat string count glue)))

(define(repeat string count glue)
    (if (= count 1) 
        string
        (string-append string glue (repeat string (- count 1) glue))))


;((repeater "I love Racket") 3 " ")
;((repeater "Quack") 5 "!")

;;; Задача 5. Да се дефинира функция (sum-sum-digit a b k), която намира сумата на
;;; естествените числа от a до b (0<a≤b), сумата от цифрите на които е кратна на k.

(define (sum-sum-digit a b k)
    (apply + (filter (lambda (x) (= (remainder (sum-digit x) k) 0)) (range a (+ b 1)))))

(define (sum-digit x)
    (define (helper x sum)
        (if(= x 0)
            sum
            (helper (quotient x 10) (+ sum (remainder x 10)))))
    (helper x 0))

;(sum-sum-digit 11 15 2)

;;; Задача 6. Да се дефинира функция (max-ordered-sublist lst), която намира най-
;;; дългия възходящо сортиран подсписък на списъка от числа lst.
;;; Пример:


(define (max-ordered-sublist lst)
  (define (helper lst res)
    (cond ((null? lst) (find-max-ordered res))
          (else (helper (cdr lst) (append res (make-sublists lst ))))))
  (helper lst null))

(define (find-max-ordered res)
    (argmax length (filter (lambda (x) (if (= (length x) 1) 
                                            x
                                            (apply <= x))) res)))

(define (make-sublists lst )
    (define (helper lst i sublists)
        (cond   ((= i (+ (length lst) 1)) sublists)
                (else (helper lst (+ i 1) (cons (take lst i) sublists )))))
    (helper lst 1 null))

 ;(max-ordered-sublist '(1 5 2 4 6 8 3 4 1)) ;→ '(2 4 6 8)

;;; Задача 7. Да се дефинира функция (pair-compose fs), която получава списък
;;; (f1 f2 f3 ... fn) от едноаргументни числови функции и връща нова едноаргументна
;;; числова функция g – такава, че оценката на (g x) е равна на сумата
;;; (f1.f2)(x)+(f3.f4)(x)+...+(fn-1.fn)(x), където “.” означава композиция на
;;; функции.
;;; Ако оригиналният списък с функции има нечетен брой елементи, то последната функция от
;;; списъка се композира с функцията идентитет, която получава един аргумент и го връща без
;;; промяна.

(define (pair-compose fs)
    (define (helper fs x result )
        (cond   ((null? fs) result)
                ((null? (cdr fs)) (+ result ((car fs) (identity x))))
                (else (helper (cddr fs) x (+ result ((car fs) ((cadr fs) x)))))))
    (lambda (x) (helper fs x 0) ))

;; (define g (pair-compose (list add1 add2)))
;;; (g 1) → (add1.add2)(1) → 4

;;; Задача 8. Да се напише функция (where list-elements list-predicates), която
;;; връща списък от всички елементи на list-elements, за които са изпълнени всички
;;; предикати в list-predicates.
;;; Примери:
;;; (where '(3 4 5 6 7 8 9 10) (list even? (lambda (x) (> x 5)))) →
;;; (6 8 10) (списък от всички елементи на дадения, които са четни числа, по-големи от 5)
;;; (where '(3 4 5 7) (list even? (lambda (x) (> x 5)))) → () (в списъка
;;; няма четни числа, по-големи от 5)


; Задача 9
; Да се напише функция (meet-twice? f g a b), която проверява дали в
; целочисления интервал [a; b] съществуват две различни цели числа x и y
; такива, че f(x) = g(x) и f(y) = g(y).

(define (meet-twice? f g a b)
    (define (helper f g a b count)
        (if(> a b)
            (> count 2)
            (helper f g (+ a 1) b (if (= (f a) (g a)) (+ count 1) count))))
    (helper f g a b 0))

; Задача 10
; Да се напише функция (max-duplicate ll), която по списък от списъци от
; цели числа ll намира най-голямото от тези числа, които се повтарят в
; рамките на списъка, в който се срещат. Ако в нито един списък няма
; повтарящи се числа, функцията да връща #f.

(define (max-duplicate xss)
    (apply max (flatten (map (lambda (xs) (duplicates xs)) xss))))

(define (duplicates xs)
    (filter (lambda (x) (count-elem x xs)) xs))

(define (count-elem x xs)
    (define (helper x xs counter)
        (cond   ((and (null? xs) (< counter 2)) #f)
                ((> counter 1) x)
                ((= x (car xs)) (helper x (cdr xs) (+ counter 1)))
                (else (helper x (cdr xs) counter))))
    (helper x xs 0))

;(max-duplicate '((1 2 3 2 5 2 5 ) (-4 -4) (5)))

; Задача 11
; Да се напише функция (check-matrix? m k), която проверява дали на всеки ред
; в дадена матрица m от цели числа има поне по едно число, кратно на k.

(define (check-matrix? matrix k)
    (false? (map (lambda (xs) (check xs k)) matrix)))

(define (check xs k)
    (define (helper xs k counter)
        (cond   ((and (null? xs) (= counter 0)) #f)
                ((and (null? xs) (> counter 0)) #t)
                ((= (remainder (car xs) k) 0) (helper (cdr xs) k (+ counter 1)))
                (else (helper (cdr xs) k counter))))
    (helper xs k 0)
)

(define (false? xs)
    (cond   ((eqv? (car xs) #f) #f)
            ((null? (cdr xs)) #t)
            (else (false? (cdr xs)))))

;;; (check-matrix? '((1 2 6)
;;;                  (3 8 9)
;;;                  (10 21 11)) 3)

; Задача 12
; Да се напише функция (longest-descending l), която намира низходящо сортиран
; подсписък на списъка от числа l с максимална дължина. Ако съществуват няколко
; такива подсписъка, функцията да върне първия отляво надясно.


(define (max-descending-sublist lst)
  (define (helper lst res)
    (cond ((null? lst) (find-max-descending res))
          (else (helper (cdr lst) (append res (make-sublists lst ))))))
  (helper lst null))

(define (find-max-descending res)
    (argmax length (filter (lambda (x) (if (= (length x) 1) 
                                            x
                                            (apply >= x))) res)))

;;; (define (make-sublists lst )
;;;     (define (helper lst i sublists)
;;;         (cond   ((= i (+ (length lst) 1)) sublists)
;;;                 (else (helper lst (+ i 1) (cons (take lst i) sublists )))))
;;;     (helper lst 1 null))

;; вече е написана във файла; не я пишем пак че има дублиране

;(max-descending-sublist '(5 3 8 6 4 2 6 9 8 7 6 12))
