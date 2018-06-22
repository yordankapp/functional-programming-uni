#lang racket
;Задача 1. Да  се  дефинира функция (define (replace lst dict)) ...), 
;която получава  списък lst (от  числа  и  символи) и асоциативен списък dict 
;(от точкови двойки от числа и символи). Процедурата трябва да върне нов списък, 
;в който елементите на lst са заменени с асоциацията им в dict, ако е имало такава, 
;иначе не се променят.
(define (find x dict)
        (cond ((null? dict) x)
              ((equal? x (car dict)) (cdar dict))
              (else (find x (cdr dict)))))
(define (replace lst dict)
    (map (lambda (x) (find x dict)) lst))

;;(replace '(1 2 3 4 5) '((1 7)(2 8)))

;Задача 2. Да  се  дефинира процедура със  функция (define (permutations lst) ...), 
;която връща списък  от  списъk съдържащ  всички пермутациина 
;елементите на списъка lst. В списъка lst няма да има повтарящи се елементи.
(define (insert-at x pos lst)
    (cond   ((null? lst) (list x))
            ((= pos 0)(cons x lst))
            (else (cons (car lst) (insert-at x (- pos 1) (cdr lst))))))

(define (in-all-positions x lst)
      (define (helper pos)
        (if (< pos 0)
            '()
            (cons (insert-at x pos lst) (helper (- pos 1)))) )
     (helper (length lst)))

(define (permutations lst)
    (define (helper rem res)
        (if (null? rem)
            res
            (helper (cdr rem) (apply append (map (lambda (lst) (in-all-positions (car rem) lst)) res)))))
    (helper (cdr lst) (list (list (car lst)))))

;(permutations '(1 2 3))

;;; Задача 3. Да се дефинира функция (max-ordered-sublist lst), която намира най-дългия възходящо сортиран подсписък на списъка от числа lst.
;;; Пример: (max-ordered-sublist '(1 5 2 4 6 8 3 4 1)) -> '(2 4 6 8)

;;; Задача 4. Да се дефинира предикат (triangular? mat), който получава квадратна числова матрица,
;;; представена като списък от списъци, и проверява дали тя е горно триъгълна, т.е. дали всичките
;;; елементи под главния й диагонал са нули.
;;; Пример 1: (triangular? '((1 2 3) (0 5 6) (0 0 9))) -> #t
;;; Пример 2: (triangular? '((0 2 3) (0 0 6) (1 0 0))) -> #f

;;; Задача 5. Да се дефинира функция (pair-compose fs), която получава списък (f1 f2 f3 ... fn) от едноаргументни числови функции и връща нова едноаргументна числова функция g - такава, че оценката на (g x) е равна на сумата (f1 . f2) (x) + (f3 . f4) (x) + ... + (fn-1 . fn) (x), където “.” означава композиция на функции. Ако оригиналният списък с функции има нечетен брой елементи, то последната функция от списъка се композира с функцията идентитет, която получава един аргумент и го връща без промяна.
;;; Пример:
;;; (define g (pair-compose (list add1 add2)))
;;; (g 1) > (add1 . add2) (1) -> 4
