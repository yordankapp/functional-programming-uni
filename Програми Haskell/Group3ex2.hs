{- 
Списъците (Lists) са един от основните съставни типове данни в Хаскел (заедно с 
векторите (Tuples), който ще разгледаме в следващото упражнение).

Списъците могат да съдържат елементи от произволен тип, но всички елементи на
даден списък ТРЯБВА да бъдат от един и същи тип. Това определя и типа на самия 
списък, напр. [Int] e списък от цели числа (Int), докато [[Double]] е списък
от списъци от числа с плаваща запетая (Double).

По конвенция, в Хаскел имената на променливите, които са списъци винаги завършват 
с буквата 's' (както думите в множествено число в английскя език).


Ето и някои основни операции върху списъци:

1. Създаване:

- директно изброяване на елементите:
  xs = [1, 2, 3, 4, 5]
  [] - създава празен списък

- дефиниране на интервал (range):
  [1 .. 10]                 -> [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

- интервал със стъпка (равна на разликата м/у първия и нулевия елементи):
  [1, 3 .. 10]              -> [1, 3, 5, 7, 9]


2. Добавяне на елементи:
- добавяне на елемент в началото:
  123 : xs                  -> [123, 1, 2, 3, 4, 5]

  Операторът (:) се нарича cons, съкратено от construct.

- слепване (конкатенация) на два или повече списъка:
  [1, 3, 5] ++ [2, 4, 6]    -> [1, 3, 5, 2, 4, 6]
  [11] ++ [22] ++ [33]      -> [11, 22, 33]

- В Хаскел няма оператор за добавяне в края на даден списък, но за целта 
  може да използвате следния "трик":
  xs ++ [345]               -> [1, 2, 3, 4, 5, 345]


2. Полезни функции: 

- null xs връща дали списъкът е празен:
  null [1, 2, 3, 4, 5]      -> False
  null []                   -> True

- length xs връща дължината на списъка:
  length [1, 2, 3, 4, 5]    -> 5
  length []                 -> 0

- head xs връща първия елемент на списъка (или грешка ако е празен!):
  head [1, 2, 3, 4, 5]      -> 1
  head []                   -> Exception: Prelude.head: empty list

- tail xs връща всички БЕЗ първия елемент:
  tail [1, 2, 3, 4, 5]      -> [2, 3, 4, 5]
  tail []                   -> []

- take n xs връща първите n елемента на xs:
  take 2 [1, 2, 3, 4, 5]    -> [1, 2]
  take 10 [1, 2, 3, 4, 5]   -> [1, 2, 3, 4, 5] - ако n >= length xs, take връща 
                                                 целия списък

- drop n xs връща всички БЕЗ първите n елемента от xs:
  drop 2 [1, 2, 3, 4, 5]    -> [3, 4, 5]
  drop 10 [1, 2, 3, 4, 5]   -> [] - ако n >= length xs, drop връща празен списък

- reverse xs връща списък с елементите в обратен ред:
  reverse [1, 2, 3, 4, 5]   -> [5, 4, 3, 2, 1]
  reverse []                -> []

- elem x xs ни казва дали x е елемент на списъка xs:
  2 `elem` [1, 2, 3, 4, 5]  -> True
  10 `elem` [1, 2, 3, 4, 5] -> False

- minimum, maximum, sum, product - правят, каквото очаквате:

  minimum [1, 2, 3, 4, 5]   -> 1
  minimum []                -> Exception: Prelude.minimum: empty list

  maximum [1, 2, 3, 4, 5]   -> 5
  maximum []                -> Exception: Prelude.maximum: empty list

  sum [1, 2, 3, 4, 5]       -> 15 = 1 + 2 + 3 + 4 + 5
  sum []                    -> 0

  product [1, 2, 3, 4, 5]   -> 120 = 1 * 2 * 3 * 4 * 5
  product []                -> 1
-}

{- Примери -}

{- Пример 1. факториел -}
factorial :: (Integral t) => t -> t
factorial n = product [1 .. n]

{- Пример 2. Обръщане на списък -}
-- чрез guards (подобно на Racket)
my_reverse' :: [t] -> [t]
my_reverse' xs 
    | null xs   = []
    | otherwise = my_reverse' (tail xs) ++ [head xs]

-- чрез съпоставяне по образец (pattern matching)
my_reverse'' :: [t] -> [t]
my_reverse'' []     = []
my_reverse'' (x:xs) = my_reverse'' xs ++ [x]

{- Пример 3. Сбор на елементите на списък -}
-- чрез guards (подобно на Racket)
my_sum' :: (Num t) => [t] -> t
my_sum' xs 
    | null xs   = 0
    | otherwise = head xs + my_sum' (tail xs)

-- чрез съпоставяне по образец (pattern matching)
my_sum'' :: (Num t) => [t] -> t
my_sum'' []     = 0
my_sum'' (x:xs) = x + my_sum'' xs



{- Задачи -}
{-
Задача 1. Да се дефинира функция digits n, която връща списък с цифрите на 
цялото число n >= 0.

Примери: 
    digits 1234 = [1, 2, 3, 4]
    digits 1750 = [1, 7, 5, 0]
-}

digits :: (Integral t) => t -> [t]
digits n 
    | n == 0    = [] 
    | otherwise = digits (n `div` 10) ++ [( n `mod` 10)] 

{-
Задача 2. Да се дефинира функция times n x, която получава цяло положително число 
n и стойност от произволен тип x и връща списък съдържащ x, повторено n пъти.

Примери:
    2 `times` 7 = [7, 7]
    7 `times` 2 = [2, 2, 2, 2, 2, 2, 2]
-}
times :: (Integral t) => t -> a -> [a]
n `times` x 
    | n == 0 = []
    | otherwise = x: (n - 1) `times` x

{-
Задача 3.
a). Напишете функцията collatz n, дефинирана по-следния начин:
    - ако n e равно на 1, функцията връща 1.
    - ако n е четно, функцията връща n делено на 2.
    - ако n е нечетно, функията връща 3n + 1.
    Параметърът n e цяло число.

Примери:
    collatz 1 = 1
    collatz 4 = 2
    collatz 3 = 10


b). Напишете функцията areCollatzSequence, която приема списък от цели числа и връща 
дали всеки от елементите (с изключение на първия) се получава посредством извикването
на функцията collatz върху предишния елемент от списъка.

Примери:
    areCollatzSequence [3, 10, 5, 16, 8, 4, 2, 1] = True
    areCollatzSequence [9, 28, 14, 7, 22, 11, 34, 17, 52, 26, 13] = True
    areCollatzSequence [1, 3, 10] = False
-}

collatz :: (Integral t) => t -> t
collatz n
    | n == 1 = 1
    | ( n `mod` 2) == 0 = n `div` 2
    | otherwise = 3*n + 1 

areCollatzSequence :: (Integral t) =>  [t] -> Bool
areCollatzSequence xs 
    | null xs = True
    | null (tail xs) = True
    | collatz (head xs) == (head (tail xs))  = areCollatzSequence (tail xs)
    | otherwise = False

{-
Задача 4. Mergesort: mergesort e един от най-ефикасните алгоритми за сортиране, особено що се отнася до 
функционални езици. Функцията sort от модула Data.List, която е стандартния начин за сортиране на списъци
в Haskell, използва - макар и малко по-оптимизиран от този, който ще имплементирате - вариант на този алгоритъм.

а). Нaпишете функцията merge xs ys, която приема два списъка подредени в нарастващ ред и ги 
обединява в един списък, чийто елементи също са подредени в нарастващ ред.

    Пример: merge [1, 3, 7] [2, 4, 6] = [1, 2, 3, 4, 6, 7] 

б). Използвайте функцията от предишната подточка и идеята, че мога да сортирам списък като го
разделя на две половини, сортирам всяка от тях поотделно и после ги обединя - което е пример за
т. нар. подход на разделяй и владей (divide and conquer) - за да напишете функция mergesort xs,
която приема списък xs и връща списък с елементите на xs сортирани в нарастващ ред.

    Пример: mergesort [2, 1, 3, 7, -16, 5] = [-16, 1, 2, 3, 5, 7]
-}
merge :: Ord a => [a] -> [a] -> [a]
merge xs ys 
    | null xs && null ys = []
    | null xs = ys
    | null ys = xs
    | (head xs) <= (head ys) = (head xs) : merge (tail xs) ys
    | otherwise = (head ys) : merge xs (tail ys)

mergesort :: (Ord a) => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort xs =  helper (take ((length xs) `div` 2) xs ) (drop ((length xs) `div` 2) xs)
    where 
        helper ::  (Ord a ) => [a] -> [a] -> [a]
        helper xs ys = merge (mergesort xs) (mergesort ys)
{-
Задача 5. Трансформация на Бъроус-Уилър (Burrows-Wheeler transform)

Трансформацията на Бъроус-Уилър, е трансформация върху символни низове, която има
интересното свойство да групира еднаквите символи в низа близо един до друг. 
Поради тази причина, тя се използвана като предварителна стъпка в алгоритмите за 
компресия на данни, както и в биоинформатиката.

Алгоритъмът за намирането и е следния:
1. Генерираме всички ротации на входния низ, което ни дава списък от низове.
2. Сортираме списъка лексикографски.
3. Взимаме последния символ от всеки от редовете.

Пример:
    "BANANA" -- rotations --> [ "BANANA", -- sort --> [ "ABANAN", -- last symbol --> "NNBAAA"
                                "ANANAB",               "ANABAN",
                                "NANABA",               "ANANAB",
                                "ANABAN",               "BANANA",
                                "NABANA",               "NABANA",
                                "ABANAN" ]              "NANABA" ]

a). Напишете функцията rotate n str, която ротира низа str с n позиции наляво.

    Пример: rotate 1 "abc" = "bca"

б). Напишете функцията rotations str, която генерира всички ротации на str.

    Пример: rotations "abc" = ["abc", "bca", "cab"]

в). Напишете функцията bwt str, която приема низа str, и връща неговата трансформация
    на Бъроус-Уилър.

    Пример: bwt "BANANA" = "NNBAAA"
-}
rotate :: Int -> [a] -> [a]
rotate n xs 
    | n == 0 = xs
    | null xs = []
    | otherwise = rotate (n - 1) ( (tail xs) ++ [(head xs)]) 

rotations :: [a] -> [[a]]
rotations xs = helper xs (length xs)
    where
        helper:: [a] -> Int -> [[a]]
        helper xs n 
            | n == 0 = []
            | otherwise = xs : helper (rotate 1 xs) (n - 1)

bwt :: Ord a => [a] -> [a]
bwt xs = map (\x -> last x) (mergesort (rotations xs)) 


-- main функция с примерни извиквания на функциите от задачите.
main = do 
    -- Примери
    print $ factorial 5
    print $ my_reverse' [1, 2, 3, 4]
    print $ my_reverse'' [1, 2, 3, 4]
    print $ my_sum' [1, 2, 3, 4]
    print $ my_sum'' [1, 2, 3, 4]

    -- Задача 1.
    print $ digits 1234
    print $ digits 1750    

    -- Задача 2.
    print $ 2 `times` 7
    print $ 7 `times` 2

    -- Задача 3.
    print $ collatz 1
    print $ collatz 4
    print $ collatz 3
    print $ areCollatzSequence [3, 10, 5, 16, 8, 4, 2, 1]
    print $ areCollatzSequence [9, 28, 14, 7, 22, 11, 34, 17, 52, 26, 13]
    print $ areCollatzSequence [1, 3, 10]

    -- Задача 4.
    print $ merge [1, 3, 7] [2, 4, 6]
    print $ mergesort [2, 1, 3, 7, -16, 5]

    -- Задача 5.
    print $ rotate 1 "abc"
    print $ rotations "abc"
    print $ bwt "BANANA"