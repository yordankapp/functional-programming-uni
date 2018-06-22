#lang racket

;;Зад 1 : Да се напише ф-ция range a b , 
;; която връща списъка ' (а а+1 ..b)

(define (my-range a b)
        (cond   ((> a b) null)
                (else (cons a (my-range (+ a 1) b)))))

;(my-range 3 5)

;; Зад 2 Да се напише ф-ция sum-odd  lst,
;;която получава списък lst и връща сумата на нечетните числа в lst

(define (sum-odd lst)
    (cond ((null? lst) 0)
            ((= (remainder (car lst) 2) 0) (sum-odd (cdr lst)))
            (else (+ (car lst) (sum-odd (cdr lst))))))

;;(sum-odd (list 3 4 5 6))

;; Зад 3 Да се напише ф-ция filter-even lst , която
;;връща нов списък съставен само от четните числа в lst

(define (filter-even lst)
    (cond ((null? lst) null)
            ( (= (remainder (car lst) 2) 0)  (cons (car lst) (filter-even (cdr lst))))
            (else (filter-even (cdr lst)))))
;;(filter-even (list 3 4 5 6))

;;Зад 4 Да се напише ф-ция intersect set1 set2 ,която 
;;намира сечението на множествата set1, set2 (представени чрез списъци)

(define (intersect set1 set2)
    (cond ((null? set1) null)
            ((null? set2) null)
            ((member (car set1) set2) (cons (car set1) (intersect (cdr set1) set2)))
            (else (intersect (cdr set1) set2))))

;;(intersect (list  2 3 4 5 12) (list 1 2 5 10))
 