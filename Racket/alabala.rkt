#lang racket

;(cdr (cadr '((a (b)) ((c (d)) e)))) 
;(cons '(a b) (list 'c '((d) e))) 
;(append '(a (b c)) (caddr '((a b) c ((d) e)))) 
;(list '(a b) (list '(c d))) 

(define (scons a xss)
    (map (lambda (xs)  (cons a xs) ) xss))

;(scons 5 '( (2 3) (5 7) (1 9)))


(map length
 (map (lambda (x)
 (cond [(not (pair? x)) (list x)]
 [(null? (cdr x)) x]
 [else (cdr x)])
 )
 '((2 3 4) (8 5) 6 (7 1 –1 5) (1))))


 (map (lambda (x)
 (cond [(not (pair? x)) (list x)]
 [(null? (cdr x)) x]
 [else (cdr x)]))
 '((2 3 4) (8 5) 6 (7 1 –1 5) (1)))