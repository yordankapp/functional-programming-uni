#lang racket

;;(define s-exp '(+ 1 2))
;;s-exp;; '(+ 1 2)
;;(eval s-exp);; 3


; I. Двойка (pair):
;  - Създава се чрез (cons a b). cons идва от `construct`
;  - Проверка дали x e двoйка (pair? x)
;  - Първи елемент: (car x)
;  - Втори елемент: (cdr x)
;
; II. Списък (list):
; А. Списъците в Scheme се дефинират като:
; 1. Празен списък: null, '()
; 2. Двойка чийто втори елемент (cdr) e списък.
;
; B. Конструиране на списъци:
; 1. cons:  ако xs e списък, то (cons x xs) добавя x в началото на xs.
; 2. list:  (list 1 2 3 4)
; 3. range: (range 1 10) -> '(1 2 3 .. 9)
; 4. quote: (quote (1 2 3 4)) -> '(1 2 3 4)
;    ' e shortcut за quoute, т.е. '(exression) <=> (quoute (exression))
;
; N.B. quote e "специална форма", докато list - просто ф-я. (т.е. quote is magic).
; Въпрос: тествайте какво се случва в долните два случая?
; > (list 1 2 (+ 1 2))
; > '(1 2 (+ 1 2))
;
; C. Основни операции върху списъци:
;  - list? xs              проверява дали xs e списък
;  - null? xs              проверява дали списъкът xs e празен
;  - car/cdr xs            връщат съответно главата и опашката на xs
;  - length xs             връща дължината на xs
;  - reverse xs            връща xs обърнат на обратно (т.е. '(1 2 3 4) -> '(4 3 2 1))
;  - member x xs           връща подсписъка на xs, който започва с x ако x се среща в xs
;                          в противен случай връща стойността #f (false)
;  - append xs ... ys      конкатенира (слепва) 2 или повече списъка
;
;  + За следващия път:
;  - apply f xs, map f xs, filter p? xs, foldl/foldr f acc xs
;  - take/drop xs n, takef/dropf xs p?
;  ...
;
; N.B. Ако функцията `fun` е част от Racket, ще наименоваме нашата имплементация `my-fun`
; N.B. По конвенция (от Haskell :)) променливите, които са списъци ще завършват на `s`,
; както думите в множествено число в анлийския език.


; Пример 1. Да се дефинира функцията (my-length xs),
; връщата броя на елементите на списъка xs.
(define (my-length xs)
  (if (null? xs)
      0
      (+ 1 (my-length (cdr xs)))))

;(my-length null)
;(my-length (list 1 2 3 4))
;(my-length (range 0 100))



;;Задача 1. Да се дефинира функцията (my-reverse xs),
;; връщата списък с елементите на xs в обратен ред.

(define (my-reverse xs)
    (define (helper res rems)
        (if (null? rems)
         res
         (helper (cons (car rems) res ) (cdr rems)))
    )
    (helper null xs) 
)


 ;(my-reverse (list 1 2 3 4)) ;; '(4 3 2 1)

;;Задача 2. Да се дефинира функцията (my-append xs ys),
;; която конкатенира списъците xs и ys.

(define (my-append xs ys)
        (cond ((null? xs) ys)
              ((null? ys) xs)
              (else (cons (car xs) (my-append (cdr xs) ys))))
)

;;(my-append null (list 1 2 3 4));; '(1 2 3 4)
;;(my-append (list 1 2 3 4) null);; '(1 2 3 4)
;;(my-append (list 1 2) (list 3 4));; '(1 2 3 4)

;; Задача 3. Да се дефинира функцията (my-remove-duplicates xs),
;; която премахва повтарящите се елементи от списъка xs.

(define (my-remove-duplicates xs)
  
    (define (helper res rems)
      (cond ((null? rems) (reverse res))
            ((member (car rems) res) (helper res (cdr rems)))
            (else (helper (cons (car rems) res) (cdr rems))))
        )
      (helper null xs)
   )

;(my-remove-duplicates null);;'()
;(my-remove-duplicates (list 1 1 1 1));;'(1)
;(my-remove-duplicates (list 1 2 3 4));;'(1 2 3 4)
;(my-remove-duplicates (list 1 2 2 4));;'(1 2 4)

;; Задача 4. Да се дефинира функцията (digits n),
;; връщаща списък с цифрите на числото n.

(define (digits n)

   (define (helper n ns)
    (cond ((= n 0) ns)
          (else (helper (quotient n 10) (cons (remainder n 10) ns))))
   )
   (if(= n 0)
       0
       (helper n null))
)

;(digits 0)
;(digits 1234)
;(digits 4321)


; Задача 5*. Бикове и крави:
; Да се дефинира функцията (bulls-and-cows secret guess),
; която приема `тайното` число secret и `предположението` guess
; и връща двойка (pair), чийто първи елемент е броя на `биковете`
; в guess (т.е. на цифрите в guess, който се срещат на същата позиция
; в secret), a втори - броя на `кравите` (цифри на guess, които
; се срещат в secret, но са на различна позиция).
;
; Примери:
; > (bulls-and-cows 1234 1234) -> '(4 . 0) ; 4 бика
; > (bulls-and-cows 1234 1324) -> '(2 . 2) ; 2 бика и 2 крави
; > (bulls-and-cows 1234 4321) -> '(0 . 4) ; 4 крави
; > (bulls-and-cows 1234 5678) -> '(0 . 0) ; няма общи цифри
; 
; N.B. Числата secret и guess ТРЯБВА да НЕ съдържат повторения и
; да бъдат с еднаква дължина.
;
; REF: https://bg.wikipedia.org/wiki/Бикове_и_крави

(define (bulls-and-cows secret guess)
    (define (helper secretlist guesslist)
         (define (bull secretlist guesslist i )
           ( cond  ((null? guesslist) i)
                   ((= (car secretlist) (car guesslist))   
                            (bull (cdr secretlist) (cdr guesslist) (+ i 1) ))
                   (else   (bull (cdr secretlist) (cdr guesslist) i ))))
         (define (cows secretlist guesslist i)
            (cond  ((null? guesslist) i)
                    ((member (car guesslist) secretlist) (cows secretlist (cdr guesslist) (+ i 1)))
                    (else (cows secretlist (cdr guesslist) i))))

         (bull secretlist guesslist 0 )
         (cows secretlist guesslist 0)

        ;;;  (cons (bull secretlist guesslist 0)
        ;;;         (- (cows secretlist guesslist 0) (bull secretlist guesslist 0)))

        
        (cond ((= (my-length secretlist) (my-length guesslist)) 
                    (cons (bull secretlist guesslist 0)
                       (- (cows secretlist guesslist 0) (bull secretlist guesslist 0))))
                (else (display 'failed) (newline))

    ))
    (helper ( my-remove-duplicates(digits secret)) (my-remove-duplicates(digits guess)))
)   
        
;;"5. Примери: bulls-and-cows"
;(bulls-and-cows 1234 1)
;(bulls-and-cows 1233 1234)
;(bulls-and-cows 1234 1111)
;(bulls-and-cows 1234 1234); (4 . 0)
;;(bulls-and-cows 1234 4321) (0 . 4)
;;(bulls-and-cows 1234 1324) (2 . 2)
;;(bulls-and-cows 1234 1485) (1 . 1)


; Задача 6*. Да се дефинира функцията (pick n xs), която връща списък с
; всички възможни избора на n елемента от списъка xs.
; - Пример: (pick 2 '(1 2 3)) -> '((1 2) (1 3) (2 3))
;
; В случая примемаме, че изборът '(1 2) е неразличим от '(2 1)
;
; Може да се опитате да решите задачата и в случая, когато ги приемаме
; за различни. Тогава:
;    (pick-ordered 2 '(1 2 3)) -> '((1 2) (2 1) (1 3) (3 1) (2 3) (3 2))
(define (pick-ordered n xs) 
    (define (helper n xs copyxs)
    )
    (helper n xs xs)
)

;"6. Примери: pick"
;;; (pick 1 null)
;;; (pick 0 (list 1 2 3))
;;; (pick 2 (list 1 2 3))

