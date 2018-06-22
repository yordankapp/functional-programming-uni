
main = do 
    print "abc"

    fact :: Int->Int
    fact 0 = 1
    fact n = n * fact (n - 1)

    f1 :: Int->Bool
    f1 _ = 0
    
  --  main :: IO()
    main = do
      print(count_digits 0)
    
    {-
      Зад. 1. Да се дефинира процедура count_digits :: Int -> Int, която генерира линейно
      рекурсивен процес и намира броя на цифрите на дадено естествено число n.
    -}
    countDigits :: Int -> Int
    countDigits n = if n < 10 then 1 else 1 + countDigits(div n 10)
    
    {-
      Зад. 2. Да се дефинира процедура sumDigits :: Int -> Int, която генерира линейно
      рекурсивен процес и намира сумата от цифрите на дадено естествено число n.
    -}
    sumDigits :: Int -> Int
    sumDigits n = if n < 10 then n else (mod n 10) + sumDigits(div n 10)

    {-
      Зад. 3. Да се дефинира процедура pow :: Double -> Int -> Double, която генерира линейно рекурсивен
      процес и намира x на степен n, където x е реално, а n - естествено число.
    -}
    pow :: Double -> Int -> Double
    pow n x = if (n==0) then 1 else x * pow (n - 1) x

    {-
      Зад. 4. Да се дефинира процедура sum_digits_iter :: Int -> Int, която генерира линейно
      итеративен процес и намира сумата от цифрите на дадено естествено число n.
    -}
    sum_digits_iter :: Int -> Int
    sum_digits_iter n = helper n 0 
        where
         helper :: Int -> Int -> Int
         helper n sum = if (n==0) then sum else sum_digits_iter (div n 10) (sum + (mod n 10))  
    
    {-
      Зад. 5. Да се дефинира процедура rev :: Int -> Int, която генерира линейно итеративен
      процес и по дадено естествено число n намира числото, записано със същите цифри,
      но в обратен ред.
    -}
    rev :: Int -> Int
    rev n = helper n 0
        where
            helper :: Int -> Int -> Int
            helper n rez = if (n==0) then rez else helper (div n 10) ((rez * 10) + (mod n 10))
    
    {-
      Зад. 6. Да се дефинира предикат prime :: Int -> Bool, който проверява дали дадено естествено
      число n е просто.
      Забележка: Числото 1 не е нито просто, нито съставно.
    -}

    prime :: Int -> Bool
    prime n = helper n 2
        where
            helper :: Int -> Int -> Bool
            helper n i 
            | n==1 = False
            | n==i = True
            | (mod n i)==0 = False
            |otherwise = helper n (i + 1)
    
   