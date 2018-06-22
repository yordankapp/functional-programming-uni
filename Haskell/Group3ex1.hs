{- ПРИМЕРИ -}

-- Пример 1. Обиколка на окръжност с радиус r.
perimeter :: Double -> Double
perimeter r = 
    if r < 0 
        then error "r >= 0" 
        else 2 * pi * r


-- Пример 2. factorial n, която пресмята факториела на целочисленият аргумент n.
factorial :: Integer -> Integer
factorial n 
    | n < 0     = error "n >= 0"            -- съобщение за грешка при неправилни входни данни 
    | n == 0    = 1                         -- базов случай
    | otherwise = n * factorial (n - 1)     -- общ случай


-- Пример 3. Провека дали цялото число k е делител на числото n:
divides :: Integer -> Integer -> Bool
k `divides` n = n `mod` k == 0


{- ЗАДАЧИ -}

{-
Задача 1. Напишете функцията max3 x y z, която връща най-голямото от числатa x, y и z.
За целта използвайте функцията max x y, която връща по-голямото от двете числа.
-}
max3 :: Int -> Int -> Int -> Int
max3 x y z = max (max x y) z

{-
Задача 2. Редица на Фибоначи: напищете функцията fib n, която връща n-тото число от редицата на
Фибоначи, дефинирана като:

    fib(0) = 0
    fib(1) = 1
    fib(n) = fib(n - 1) + fib(n - 2)

    Пример:
        fib 3 = 2
        fib 8 = 21
-}
fib :: Integer -> Integer
fib n 
    | n == 0 = 0
    | n == 1 || n==2  = 1
    | otherwise = fib (n - 1) + fib (n - 2)

{-
Задача 3. Прости числа: дефинирайте функцията isPrime n, която приема цялото число n и проверява 
дали то е просто.
-}
-- Идеята е да итерираме по числата от 2 до sqrt(n) и да проверяваме дали n се дели на някое от тях.
-- За целта имаме нужда от още една променлива (подобно на броячът на цикъл в С++).
-- Начинът, по който се прави това в Хаскел е чрез дефиниране на помощна функция която приема един допълнителен
-- параметър, в който ще пазим стойността на съответния "брояч".
isPrime :: Integer -> Bool
isPrime n = undefined
--вече писана

{-
Задача 4. Да се дефинира функцията isNarcissistic n, която приема като аргумент цялото положително
число n и връща дали то е нарцистично. Нарцистични се наричат числата, които са равни на сбора на 
цифрите си (в десетична бройна система), всяка повдигната на степен броя на цифрите на числото.

Пример за такова число е 153, тъй като 1 ^ 3 + 5 ^ 3 + 3 ^ 3 = 1 + 125 + 27 = 153. 
-}
isNarcissistic :: Integer -> Bool
isNarcissistic n = helper n n 0 (countDigit n)
    where 
        helper :: Integer -> Integer -> Integer -> Integer -> Bool
        helper n copyN sum count
            | (n == 0) && (copyN == sum)  = True
            | (n == 0) && (copyN /= sum)  = False
            | otherwise = helper (div n 10) copyN (sum + ((mod n 10) ^ count)) count

countDigit :: Integer -> Integer
countDigit n = if (n < 10) then 1 else (1 + countDigit (div n 10))

{-
Задача 5. Да се дефинира функцията isPerfectNumber n, която приема целочисления аргумент n и
връща дали той е перфектно число. Перфектно число се нарича всяко чяло число, равно на сбора на
собствeните си делители.

Примери: 
    6 = 1 + 2 + 3
    28 = 1 + 2 + 4 + 7 + 14
-}
isPerfectNumber :: Integer -> Bool
isPerfectNumber n = helper n 0 1
    where
        helper :: Integer ->Integer -> Integer ->Bool
        helper n sum i
            | n == i  && n == sum   = True
            | n == i && n/= sum     = False
            | (mod n i) == 0        = helper n (sum + i) (i + 1)
            | otherwise             = helper n sum (i + 1)

{-
Задача 6. Напишете оператор n ## k, който взима n > 0 и k >= 0 и връща сумата от всяка цифра
на n повдигната на степен k.

    Пример: 
        12 ## 2 = 1 ^ 2 + 2 ^ 2 = 1 + 4   = 5
        17 ## 3 = 1 ^ 3 + 7 ^ 3 = 1 + 343 = 344
-}
(##) :: Integer -> Integer -> Integer   -- Когато пишем типа на оператора, ограждаме името му със скоби.
n ## k =  helper n k 0
    where
        helper n k sum
            | n == 0  = sum
            | otherwise = helper (div n 10) k (sum + ((mod n 10) ^ k))

-- main функция с примерни извиквания
main = do 
    -- Примери:
    print $ perimeter 10
    print $ factorial 5
    print $ 3 `divides` 7
    print $ 3 `divides` 9

    -- Задача 1.
    print $ max3 3 1 7
    print $ max3 3 12 7
    print $ max3 50 1 7

    -- Задача 2.
    print $ fib 3
    print $ fib 8

    -- Задача 3.
    -- print $ isPrime 2
    -- print $ isPrime 4
    -- print $ isPrime 97

    -- Задача 4.
    print $ isNarcissistic 153
    print $ isNarcissistic 28

    -- Задача 5.
    print $ isPerfectNumber 6
    print $ isPerfectNumber 28
    print $ isPerfectNumber 42

    -- Задача 6.
    print $ 12 ## 2
    print $ 17 ## 3

    