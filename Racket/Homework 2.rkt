#lang racket

(define (sum-numbers string)
    (define (helper string sum)
        (cond   ((null? string ) sum)
                ((char-numeric? (car string)) 
                    (helper (repeat-cdr string (count-digit-number string)) 
                            (+ sum (find-number string (count-digit-number string)))))
                (else (helper (cdr string) sum))))
    (helper (string->list string) 0))

(define (repeat-cdr xs counter)
    (if(= counter 0)
        xs
        (repeat-cdr (cdr xs) (- counter 1))))

(define (find-number xs digits)
    (define (helper xs digits number)
        (if (= digits 0)
            number
            (helper (cdr xs) (- digits 1) 
                (+ number (* (- (char->integer (car xs)) (char->integer #\0)) (expt 10 (- digits 1 )))))))
    (helper xs digits 0))

(define (count-digit-number xs)
    (define (helper xs counter)
        (cond   ((null? xs) counter)
                ((not (char-numeric? (car xs))) counter)
                (else (helper (cdr xs) (+ counter 1)))))
    (helper xs 0))

