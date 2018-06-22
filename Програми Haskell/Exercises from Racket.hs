
import Data.List (subsequences)  

-- Да се дефинира функция,която проверява дали числото n e съвършено, 
--т.е. дали е равно на сбора на делителите си.
isPerfectNumber :: Int -> Bool
isPerfectNumber n = if ((sum (findDivisors n 1)) == n) then True else False
    where 
        findDivisors :: Int ->Int -> [Int]
        findDivisors n i 
         | n == (i+1)  = []
         | n `mod`i == 0  = i: findDivisors n (i+1)
         | otherwise = findDivisors n (i+1)

--Да се дефинира функция,която проверява дали цифрите на числто n са подредени в нарастващ ред.
isIncDigits :: Int -> Bool
isIncDigits n = helper n ((n `div` 10) `mod` 10) (n `mod` 10)
    where
        helper :: Int -> Int -> Int-> Bool
        helper n beforedigit digit
         | beforedigit == 0 = True
         | digit >= beforedigit  = helper (n `div` 10) ((n `div` 100) `mod` 10) beforedigit
         | otherwise = False

-- Да се дефинира функция, която приема число n и проверява n^2 завършва с цифрите на n 
isAutomorphic:: Int -> Bool
isAutomorphic n = helper n (n*n)
    where 
        helper:: Int -> Int -> Bool
        helper n squareN
         | n==0 = True
         | (squareN  `mod` 10) ==  (n `mod` 10) = helper (n  `div` 10) (squareN  `div` 10)
         | otherwise = False

-- Да се дефинира функция, която проверява дали числото n, е мерсеново просто число
-- т.е. дали е просто число от вида 2^k - 1
isMersennePrime :: Integer -> Bool

isMersennePrime n = if ((prime (2^n - 1)) == True) then True else False
        where
            prime::Int->Bool
            prime x= helper 2 x
                where
                helper:: Int -> Int-> Bool
                helper counter x  
                 | x == counter  = True
                 | x`mod`counter/=0   =  helper (counter+1) x
                 | otherwise = False

-- Да се дефинира функция, която премахва повтарящите се елементи от списъка xs.
removeDuplicates :: (Eq a) => [a] -> [a]
removeDuplicates [] = []
removeDuplicates xs = helper (head xs) xs
    where
        helper:: (Eq a) => a -> [a] -> [a]
        helper x xs
         | null xs = []
         | (countDuplicate x (tail xs) 1)== 1  = x: helper (head (tail xs)) (tail xs)
         | otherwise  = helper (head (tail xs)) (tail xs)
            where 
                countDuplicate :: (Eq a) => a -> [a] -> Integer -> Integer
                countDuplicate x xs counter 
                 | null xs = counter
                 | x==(head xs) = countDuplicate x (tail xs) (counter + 1)
                 | otherwise = countDuplicate x (tail xs) counter

-- Бикове и крави: Да се дефинира функция,която приема `тайното` число secret и `предположението` guess
-- и връща двойка (pair), чийто първи елемент е броя на `биковете`
-- в guess (т.е. на цифрите в guess, който се срещат на същата позиция в secret),
-- a втори - броя на `кравите` (цифри на guess, които се срещат в secret, но са на различна позиция).
bullsAndCows :: Integer -> Integer -> (Integer,Integer)
bullsAndCows secret guess =  let fst = (countBulls secret guess 0)
                                 snd = (countCows secret guess 0)
                             in (fst,snd)
  where
    countBulls ::Integer -> Integer -> Integer -> Integer
    countBulls secret guess counter 
     | guess==0 = counter
     | (secret `mod` 10) == (guess `mod` 10) = countBulls (secret `div` 10) (guess `div` 10) (counter + 1)
     | otherwise = countBulls ( secret `div` 10) (guess `div` 10) counter

    countCows :: Integer -> Integer -> Integer -> Integer
    countCows secret guess counter 
     | guess==0 = counter
     | helper (guess `mod` 10) secret  = countCows secret (guess `div` 10) (counter + 1)
     | otherwise = countCows secret (guess `div` 10) counter

      where
        helper :: Integer -> Integer -> Bool
        helper x secret 
         | secret==0 = False
         | x == (secret `mod` 10) = True
         | otherwise = helper x (secret `div` 10)


-- Да се дефинира функция, която връща списък с всички възможни избора на n елемента от списъка xs.
-- Пример: pick 2 [1, 2, 3] -> [(1 2) (1 3) (2 3)]
-- В случая примемаме, че изборът '(1 2) е неразличим от '(2 1)
pick :: Int -> [Int] -> [[Int]]
pick n xs = filter (\x -> length x == n ) (subsequences xs)

-- Да се дефинира функция, която връща броя на срещанията на най-малкия елемент на списъка xs в него.
countMinimum :: [Integer] -> Int
countMinimum xs = length (filter (==minimum xs) xs)

-- Да се дефинира функция, която по зададени списъци xs и subxs връща като резултат броя на
-- срещанията на списъка subs в списъка xs.
countOccurrences :: [Int] -> [Int] -> Int
countOccurrences xs subs = length (filter (\x -> x == subs) (helper (length subs) xs))
    where
        helper:: Int -> [Int] -> [[Int]]
        helper lenSubs xs = if ( (length xs) < lenSubs) then [] else (take lenSubs xs) : (helper lenSubs (tail xs)) 

-- Разлагане на прости делители: Да се дефинира функция, която приема целочисления аргумент n 
--и връща списък от двойки (pairs) от тип (pi . ki), където pi e i-тия прост делител на n, a
-- ki степента на pi във факторизацията на n.
-- primeFactors :: Integer -> [(Integer,Int)]
-- primeFactors n =  [ ((head x),i)|  x <- group (findAllPrimeDivisors n 2), i= length ]
--     where 
--         findAllPrimeDivisors :: Integer-> Integer -> [Integer]
--         findAllPrimeDivisors n i
--          | n==0 = []
--          | prime i && n `mod` i==0  = i: (findAllPrimeDivisors (n `div` i) i)
--          | prime i && n `mod` i==0  = i: (findAllPrimeDivisors (n `div` i) i)
--          | otherwise = 
prime::Int->Bool
prime 1 = False
prime x = helper 2 x
    where
      helper:: Int -> Int-> Bool
      helper counter x  
       | x== counter  = True
       | x`mod`counter/=0   =  helper (counter+1) x
       | otherwise = False

-- (prime-factors 10) → '((2 . 1) (5 . 1))          ;  10 = 2^1 * 5^1
-- (prime-factors 360) → '((2 . 3) (3 . 2) (5 . 1)) ; 360 = 2^3 * 3^2 * 5^1

--Да се дефинира функция, която връща сбора на квадратите на нечетните числа в списъка xs.
sumOfOddSquares :: [Integer] -> Integer
sumOfOddSquares xs =  sum (map (\x -> x*x) (filter odd xs))

-- Да се дефинира функция, която за даден списък от числа xs връща като резултат списък с двойки (xi ni),
-- където xi е i-тият елемент на xs, а ni е броят на елементите на xs, по-големи от xi
numBiggerElements:: [Integer] -> [(Integer,Integer)]
numBiggerElements xs = map (\x -> let i = (countBiggerElements x xs 0) in (x,i)) xs
    where
        countBiggerElements:: Integer->[Integer]-> Integer-> Integer
        countBiggerElements x xs i 
         | null xs = i
         | x < (head xs) = countBiggerElements x (tail xs) (i+1)
         | otherwise =  countBiggerElements x (tail xs) i
-- Да се дефинира функция,която по списък от списъци от цели числа xss намира сумата на тези от числата,
-- които са уникални в рамките на списъка, в който се срещат.
sumUnique :: [[Integer]] -> Integer
sumUnique xss = sum (map sum (map (\xs -> filter (\x -> (countElement x xs 0) == 1 ) xs) xss))
    where
        countElement::Integer -> [Integer] -> Integer -> Integer
        countElement x xs i
         | null xs = i
         | x == (head xs) = countElement x (tail xs) (i+1)
         | otherwise = countElement x (tail xs) i

-- Да се дефинира функция, която приема целочислен аргумент n и връща единичната матрица
-- с размер n (представена като списък от n линии, всеки -списък от n елемента).  
idMatrix :: Int->[[Int]]
idMatrix n = map (\pair -> (addOne (fst pair) (snd pair) 1)) (helper (makeMatrix n n) 1) 
   where
    makeMatrix::Int-> Int-> [[Int]]
    makeMatrix n i
     | i==0 = []
     | otherwise = (replicate n 0) : (makeMatrix n (i-1)) 

    helper::[[Int]] -> Int -> [(Int,[Int])]
    helper matrix i 
     | null matrix = []
     | otherwise = (i,(head matrix)) : (helper (tail matrix) (i+1))

    addOne:: Int-> [Int] -> Int-> [Int]
    addOne numberRow row i
     | null row = []
     | numberRow==i = 1: (addOne numberRow (tail row) (i+1))
     | otherwise = (head row) : (addOne numberRow (tail row) (i+1))

-- Да се дефинира функция, която приема матрица xss (представена като списък от списъци) 
-- и връща транспонираната на xss матрица.  
transpose::[[Int]] -> [[Int]]
transpose xss 
 | null (head xss) = []
 | otherwise = (concat (map (\xs -> (filter (==head xs) xs)) xss)) : (transpose (map (\xs -> (tail xs)) xss))

--Да се дефинира функция, която приема матриците xss и yss (представени като списъци от списъци)
-- и връща тяхното матрично произведение.
matrixMultiply :: [[Int]] -> [[Int]] -> [[Int]]
matrixMultiply xss yss = helper xss (transpose yss)
    where
        helper::[[Int]] -> [[Int]] -> [[Int]]
        helper xss yss
         | null xss = []
         | otherwise = (calculateRow (head xss) yss) : helper (tail xss) yss
            where
                calculateRow::[Int]->[[Int]] -> [Int]
                calculateRow xs yss = map (\ys -> (calculateElement xs ys 0) ) yss
                 where
                    calculateElement :: [Int] -> [Int] -> Int -> Int
                    calculateElement xs ys sum
                     | null xs = sum
                     | otherwise = calculateElement (tail xs) (tail ys) (sum + ((head xs)*(head ys)))

-- Да се дефинира функция, която връща като списък от двойки декартовото произведение на 
-- множествата xs и ys, представени чрез списъци.
cartesianProduct::[Integer] -> [Integer] -> [(Integer,Integer)]
cartesianProduct xs ys = concat (map (\x -> (map (\y -> (x,y)) ys) ) xs)

-- Да се дефинира функция, която проверява дали списъкът от списъци xss е мaтрица 
-- (т.е. дали всичките му подсписъци/редове са с еднаква дължина
isMatrix :: [[Int]] -> Bool
isMatrix xss = if length (filter (== head (map (\xs -> length xs) xss)) (map (\xs -> length xs) xss)) == (length (map (\xs -> length xs) xss)) 
               then True else False

--Да се дефинира функция, която проверява дали матрицата xss е латински квадрат. 
--Можете да приемете, че за символи ще използваме числата от 1 до n, където n е големината на матрицата.
isLatinSquare :: [[Int]] -> Bool
isLatinSquare xss = (checkRow xss) && (checkRow (transpose xss))
    where
        checkRow::[[Int]] -> Bool
        checkRow xss = length (filter (==False) (map (\xs -> isElem (head xs) (tail xs)) xss)) == length xss
            where
                isElem::Int -> [Int] -> Bool
                isElem x xs
                 | null xs = False
                 |  x `elem` xs = True
                 | otherwise = isElem (head xs) (tail xs)
-- Дефинирайте следните функции от по-висок ред:
-- а). (fmin f g), която приема две едноместни числови функции f и g и връща едноместни числова
-- функция, чиято стойност в точка x е минимума на f и g.
-- б). (fmax f g), като fmin, но връща максимума на f и g.
-- в). (favg f g), като fmin, но връща средното аритметично на f и g.   
fmin::(Ord a) => (a->a) -> (a->a) -> (a->a)
fmin f g = (\x -> (min (f x ) (g x)))  

fmax::(Ord a)  => (a->a) -> (a->a) -> (a->a)
fmax f g = (\x -> (max (f x) (g x)))

favg::(Ord a, Integral a)  => (a->a) -> (a->a) -> (a->a)
favg f g = (\x -> ((f x) + (g x))  `div` 2 )

--Дефинирайте следните функции от по-висок ред:
--а). (boundUp f upper), която приема едноместнa числова функция f и числова стойност up и връща
-- едноместнa числова функция, чиято стойност в точка x е минимума на f(x) и up.
-- б). (boundDown f lower), същата като bound-up, но връща максимума на f(x) и down.
boundUp:: (Ord a) => (a->a) -> a -> (a->a)
boundUp f upper = (\x -> min (f x) upper) 

boundDown:: (Ord a) => (a->a) -> a -> (a->a)
boundDown f lower = (\x -> max (f x) lower) 

-- Дефинирайте функция, която приема списък от точки в равнината (представени чрез двойки (x . y)) и връща
-- едноаргументна функция, чиято стойност в дадена точка p e най-близката до p точка от xys.
-- closestPoint :: ( Ord a)=> [(a,a)] -> (a->a)
closestPoint xys = (\(px,py) -> minimum (map (\xy -> (distance xy px py)) xys))
     where
--         distance :: (Floating a) => (a,a) -> a-> a -> a
         distance xy px py =  sqrt ((fst xy - px)*(fst xy - px) + (snd xy - py)*(snd xy - py))


-- Дефинирайте функция, която приема списъс с ребрата edges на даден ориентиран граф (в който всяко
-- ребро е представено като двойка (from . to)) и връща списък,съдържащ всички върхове на съответния граф.
nodes:: (Eq a)=> [(a,a)] -> [a]
nodes edges = removeDuplicates (concat (map (\(from,to) -> [from,to]) edges))

--Дефинирайте функция, която приема списъk с ребрата edges на даден ориентиран граф (в който всяко
-- ребро е представено като двойка (from . to)) и връща списъка на наследниците на съответния граф.
adjacencyList :: (Eq a)=>[(a,a)] -> [[(a,a)]]
adjacencyList edges = map (\node -> (children node edges)) (nodes edges)
    where
        children :: (Eq a) => a -> [(a,a)] -> [(a,a)]
        children node edges = filter (\edge -> node == (fst edge)) edges 

--Дефинирайте функция, която връща най-голямото просто число по-малко или равно на n.

maxPrime :: Int -> Int
maxPrime n = maximum [x | x <- [1..n], prime x]   

--Дефинирайте функция, която приема n-мерна точка c (представена като списък от компоненти) и число r, и връща
-- функция, чиято стойност в дадена точка p е дали p се намира вътре в n-мерната сфера с център c и радиус r.
--isInsideCirlce :: (Eq a) => (a,a) -> a -> (a->Bool) 
isInsideCirlce c r = (\(px,py) -> (distance c px py) <= r )
    where
       -- distance :: (Floating a) => (a,a) -> a -> a -> a
        distance c px py = sqrt ( (px - (fst c))*(px - (fst c)) + (py - (snd c))*(py - (snd c)) ) 

-- Дефинирайте функция, която приема списък от числа fs, съответсващи на стойностита на дадена непрекъсната 
--функция f в интервала [0 .. n] и връща минималния брой пъти, които f пресича абсцисата в дадения интервал.
countMinCrosses :: [Integer] -> Integer
countMinCrosses fs = helper fs 0
    where
        helper :: [Integer] -> Integer -> Integer
        helper fs counter 
         | null fs = counter
         | head fs == 0 = helper (tail fs) (counter+1)
         | null (tail fs) = counter
         | (head fs) * (head(tail fs)) < 0 = helper (tail fs) (counter+1)
         | otherwise = helper (tail fs) counter

-- Дефинирайте функция,която приема матрица xss (представена като списъс от списъци) и индекс на колона i 
--(започващ от нула) и обръща елементите и връща матрица, в която елементите на i-тата колона са обърнати.
-- reverseColumn :: Integer -> [[Integer]] -> [[Integer]]
-- reverseColumn i xss = let takeColumn = map (\xs -> takeElement i xs) xss.
--                           reverseColumn = reverse takeColumn
--                           newMatrix = makeNewMatrix reverseColumn xss i
--                       in newMatrix
--     where
--         takeElement :: Integer -> [Integer] -> Integer
--         takeElement i xs 
--          | i - 1 == 0  = (head xs)
--          | otherwise = takeElement (i-1) (tail xs)

--         makeNewMatrix:: [Integer] -> [[Integer]] -> Integer -> [[Integer]]
--         makeNewMatrix column xss i 
--          | null column = []
--          | otherwise = insertInRow (head xss) (head column) i : makeNewMatrix (tail column) (tail xss) i
--             where
--               insertInRow :: [Integer] -> Integer -> Integer ->[Integer]
--               insertInRow xs x i
--                 | i-1 == 0 = x : (tail xs)
--                 | otherwise = (head xs) : insertInRow (tail xs) x (i-1)
        
reverseColumn i xss =  transpose (reverseIRow (transpose xss) i)
    where
        reverseIRow xss i 
         | i-1==0 = (reverse (head xss)) : (tail xss)
         | otherwise = (head xss) : reverseIRow (tail xss) (i-1)


--Дефинирайте функция, която приема числова квадратна матрица matrix и връща
--скаларното произведение на двата и диагонала.
diagonalProduct :: [[Integer]] -> Integer
diagonalProduct xss = let  lengthMatrix = length xss
                           findMainDiagonal = mainDiagonal xss lengthMatrix 1
                           reverseRowsMatrix = map (\xs -> reverse xs) xss
                           findSecondDiagonal = mainDiagonal reverseRowsMatrix lengthMatrix 1
                           calculateProduct = calculate findMainDiagonal findSecondDiagonal
                        in calculateProduct
    where
        mainDiagonal::[[Integer]] -> Int -> Int -> [Integer]
        mainDiagonal xss count i
         | count == 0 = []
         | otherwise = (takeElementFromRow (head xss) i) : mainDiagonal (tail xss) (count -1) (i+1)
            where
                takeElementFromRow :: [Integer]  -> Int -> Integer
                takeElementFromRow xs i
                 | i - 1 == 0 = (head xs)
                 | otherwise = takeElementFromRow (tail xs) (i-1)

        calculate :: [Integer] -> [Integer] -> Integer
        calculate [] [] = 0
        calculate (x:xs) (y:ys) = x*y + calculate xs ys 

--Да се дефинира функция, която намира броя на палиндромите в интервала [a, b],
-- където a и b са цели неотрицателни числа и a<b.
palindrom::(Integer,Integer)->[Integer]
palindrom pair = filter (\x -> x==rev x 0) [(fst pair)..(snd pair)]
        where 
            rev :: Integer ->Integer-> Integer
            rev x rez
             | x==0 = rez 
             | otherwise = rev (x `div` 10) (rez*10 + x `mod` 10)

--Да се дефинира функция, която намира най-големия общ делител на две естествени числа.
nod:: Integer->Integer-> Integer
nod a b = if b==0 then a else nod b (a `mod` b)

-- Да се дефинира предикат, който проверява дали числото a се съдържа в числото b (дали a е подниз на b).
isInclude :: Int -> Int -> Bool
isInclude a b =  if length (filter (\x -> x==a) (makeList b (countDigits a 0))) >= 1  then True else False
    where
        makeList :: Int -> Int ->[Int]
        makeList b count 
         | b == 0 = []
         | otherwise = (b `mod` (10 ^ count)) : (makeList (b `div` 10) count)

        countDigits :: Int -> Int -> Int
        countDigits x i
         | x==0 = i
         | otherwise = countDigits (x `div` 10) (i + 1)

--  Да се дефинира процедура от по-висок ред, която намира първа производна на 
-- едноаргументната реална функция f с точност eps.
derive :: (Fractional a)=> (a->a) -> a -> (a->a)
derive f eps = (\x -> (((f (x + eps)) - (f x)) / eps) )

-- Да се дефинира процедура от по-висок ред (derive-n f n eps), 
--която намира n-та производна на едноаргументната реална функция f с точност eps.
deriveN :: (Fractional a, Eq a) => (a->a) -> a -> a -> (a->a)
deriveN f n eps
 | n==0 = f
 | otherwise = deriveN (derive f eps) (n-1) eps

-- Да  се  дефинира функция, която получава списък lst (от  числа  и  символи) и асоциативен списък 
--dict (от точкови двойки от числа и символи). Процедурата трябва да върне нов списък,
-- в който елементите на lst са заменени с асоциацията им в dict, ако е имало такава, иначе не се променят.
replace :: (Eq a)=> [a] -> [(a,a)] -> [a]
replace xs dict = map (\x -> findAndReplace x dict) xs
    where
        findAndReplace :: (Eq a) => a -> [(a,a)] -> a
        findAndReplace x dict
         | null dict = x
         | x == fst (head dict) = snd (head dict)
         | otherwise = findAndReplace x (tail dict)
-- Да  се  дефинира функция, която връща списък от списъци, съдържащ всички пермутациина елементите
-- на списъка lst. В списъка lst няма да има повтарящи се елементи.
-- permutations :: [a]->[[a]]
-- permutations xs = map (\x -> findPermutations x xs) xs
--         where
--             findPermutations::Integer -> [Integer]

--Да се дефинира функция, която намира най-дългия възходящо сортиран подсписък на списъка от числа lst.
maxOrderedSublist ::  [Int] -> [Int]
maxOrderedSublist xs = (maxSublist (ascendingLists xs) (maximum (map (\x -> length x) (ascendingLists xs))))
    where
        ascendingLists :: [Int] -> [[Int]]
        ascendingLists xs 
         | null xs = []
         | otherwise = ( helper (head xs) (tail xs) ) : (ascendingLists (tail xs))
            where 
                helper :: Int -> [Int] -> [Int]
                helper x xs 
                 | null xs = x : []
                 | x > (head xs) = x : []
                 | otherwise = x: (helper (head xs) (tail xs))

        maxSublist :: [[Int]] -> Int -> [Int]
        maxSublist xss maxLength 
         | maxLength == length (head xss) = (head xss)
         | otherwise = maxSublist (tail xss) maxLength
--Да се дефинира предикат, който получава квадратна числова матрица, представена като списък от списъци
--и проверява дали тя е горно триъгълна, т.е. дали всичките елементи под главния й диагонал са нули.
isTriangular :: [[Int]] -> Bool
isTriangular matrix =  let bool = helper (tail matrix) 1 
                        in allFalse bool
    where
        helper:: [[Int]] -> Int -> [Bool]
        helper matrix  i 
         | null matrix = []
         | otherwise = (checkRow (head matrix) i 0) : (helper (tail matrix) (i+1))
            where
                checkRow:: [Int] -> Int -> Int-> Bool
                checkRow xs i count
                 | i == count = True
                 | (head xs) == 0 = checkRow (tail xs) i (count + 1)
                 | otherwise = False
        allFalse :: [Bool] -> Bool
        allFalse bool
         | null bool = True
         | (head bool) = allFalse (tail bool)
         | otherwise = False

-- Да се дефинира функция , която получава списък (f1 f2 f3 ... fn) от едноаргументни числови функции
-- и връща нова едноаргументна числова функция g - такава, че оценката на (g x) е равна
-- на сумата (f1 . f2) (x) + (f3 . f4) (x) + ... + (fn-1 . fn) (x), където “.” означава композиция на функции. 
--Ако оригиналният списък с функции има нечетен брой елементи, то последната функция от списъка се композира 
--с функцията идентитет, която получава един аргумент и го връща без промяна.
-- pairCompose :: [(a->a)] -> (a->a)
-- pairCompose fs = (\x -> calculateComposition fs x 0)
--     where
--         calculateComposition:: [(a->a)] -> a-> (a->a)
--         calculateComposition fs x g
--          | null fs = g
--          | null (tail fs) = calculateComposition (tail (tail fs)) x (g + ((head fs) . (id x)) x )
--          | otherwise = calculateComposition (tail (tail fs)) x (g + ((head fs) . (head (tail fs))) x) 

main = do
   
   
   