#lang racket

(define (flatten xss)
    (cond   ((null? xss) '())
            ((list? (car xss)) (append (flatten (car xss)) (flatten (cdr xss))))
            (else (cons (car xss) (flatten (cdr xss))))))

;(flatten '(1 (2 (3 4) 5) (6 7))) ; -> (1 2 3 4 5 6 7)

;;да се намери скаларното произведение на ел-тите по главните диагонали

;;; ;;; (get-pairs '((1 2 3)
;;; ;;;           (4 5 6)
;;; ;;;           (7 8 9))) ;-> 1*3+5*5+7*9

(define (get-pairs matrix)
  (apply + (map * 
                (map (lambda (x) (list-ref (list-ref matrix x) x)) (range 0 (length matrix)))
                (map (lambda (x) (list-ref (list-ref matrix x) (- 2 x))) (range 0 (length matrix))))))

(get-pairs '((1 2 3)
          (4 5 6)
          (7 8 9)))

;;транспониране на матрица

(define (transpose matrix)
    (cond   ((null? matrix) '())
            ((null? (car matrix)) '())
            (else (cons (map (lambda (xs) (car xs)) matrix) 
                    (transpose (map (lambda (xs) (cdr xs)) matrix))))))

(transpose '((1 2 3)
            (4 5 6)
            (7 8 9)))
