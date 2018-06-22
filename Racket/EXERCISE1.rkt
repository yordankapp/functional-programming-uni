#lang sicp

;;Задача 1. Да се дефинира функция, която намира сумата на нечетните числа в затворения интервал [a, b].

(define (sum-odd a b) 
    (cond ((> a b) 0)
          ( (= (remainder a 2) 0) (sum-odd (+ a 1) b))
          (else (+ a (sum-odd (+ a 1) b)))
    )
)

;;(sum-odd 2 5);;8
;;(sum-odd 3 8);;15

;;Задача 2. Да се дефинира предикат, който проверява дали естественото число n е просто. 
;;вече написана

;;Задача 3. Да се дефинира функция, която намира сумата на първите n на брой прости числа,
;; които са по-големи от k.

;;Задача 4. Да се дефинира функция, която намира броя на палиндромите в интервала [a, b], 
;;където a и b са цели неотрицателни числа и a<b.

(define (reverse n)
    (define (help n res)
        (if (< n 10)
            (+ (* res 10) n)
            (help (quotient n 10 ) (+ (* res 10) (remainder n 10))))
    )
    (help n 0))

(reverse 186) ;;681

(define (sum-palindroms a b )
    (define (helper sum a b)
        (if (> a b)
            sum
             (if (= a (reverse a))
                 (+ a (sum-palindroms (+ a 1) b))
                 (sum-palindroms (+ a 1) b) )  
        )
    )
    (helper 0 a b)
)

;;(sum-palindroms 21 41) ;;55 

(define (count-palindroms a b)
    (define (helper count a b)
        (if (> a b)
            count
            (if(= a (reverse a))
                (helper (+ count 1) (+ a 1) b)
                (helper count (+ a 1) b))
        )
    )
    (helper 0 a b))

    ;;(count-palindroms 10 30)

;;Задача 5. Да се дефинира функция, която чрез линейно итеративен процес 
;;намира броя на естествените делители на едно естествено число.

(define (count-divisors n)
    (define (helper count i n)
        (if(> i n)
            count
            (if (= (remainder n i) 0)
                (helper (+ count 1) (+ i 1) n)
                (helper count (+ i 1) n))    
        )
    )
    (helper 0 1 n)
)

(count-divisors 15) ;;4

;;Задача 6. Да се дефинира функция, която чрез линейно рекурсивен процес 
;;намира най-големия общ делител на две естествени числа.
;;вече написана