
import Data.List
import Data.Ord

-- Задача 1. Да се напише на езика Haskell функция reverseOrdSuff :: Int -> Int, която по
-- дадено естествено число k намира число, получено от цифрите на най-дългия строго низходящ
-- суфикс на k, взети в обратен ред.
reverseOrdSuff :: Int -> Int
reverseOrdSuff k = helper k 0
    where
        helper :: Int -> Int -> Int
        helper k rezult
            | k == 0 = rezult 
            | 0<k && k<=9 = k
            | k `mod` 10 < ((k `div` 10) `mod` 10) = helper (k `div` 10) (rezult*10 + k `mod` 10) 
            | otherwise = rezult*10 + k `mod` 10

-- Задача 2. Да се напише на Haskell функция sumUnique :: [[Int]] -> Int, която по списък
-- от списъци от цели числа намира сумата на тези от числата, които са уникални в рамките на
-- списъка, в който се срещат.
sumUnique :: [[Int]] -> Int
sumUnique xss = sum (concat (map (\xs -> findUnique xs xs) xss))
    where
        findUnique :: [Int] -> [Int] ->[Int]
        findUnique xs copyxs
         | null xs = []
         | (countElem (head xs) copyxs 0) > 1 = findUnique (tail xs) copyxs
         | otherwise = (head xs) : (findUnique (tail xs) copyxs)
            where
                countElem :: Int -> [Int] -> Int -> Int
                countElem x xs counter
                 | null xs = counter
                 | x == (head xs) = countElem x (tail xs) (counter + 1)
                 | otherwise = countElem x (tail xs) counter 
-- Задача 3. Продукт се представя с наредена двойка от вида (име, цена). Наличността в даден
-- магазин се представя със списък от продукти.
-- type Product = (String,Double)
-- type StoreAvailability = [Product]
-- а) Да се напише на Haskell функция
-- closestToAverage :: StoreAvailability -> String, която намира името на продукта,
-- чиято цена е най-близка до средната цена за всички продукти. Ако има повече от един такъв
-- продукт, функцията да връща името на кой да е от намерените.
-- б) Да се напише на Haskell функция
-- cheaperAlternative :: StoreAvailability -> Int, която намира броя на продуктите,
-- за които има продукт със същото име, но по-ниска цена.

type Product = (String,Double)
type StoreAvailability = [Product]
-- import Data.List
closestToAverage ::  StoreAvailability -> String
closestToAverage xs = let average = findAverage xs 0 0
                          prices = sort (map (\x -> snd x) xs)
                          minClosestPrice = maximum (filter (<= average) prices)
                          maxClosestPrice = minimum (filter (>= average) prices)
                          wantedPrice = if (average - minClosestPrice < maxClosestPrice - average) then minClosestPrice  else maxClosestPrice
                          wantedProduct = fst (head (filter (\x -> (snd x) == wantedPrice) xs))
                        in wantedProduct
    where 
        findAverage :: StoreAvailability -> Double -> Double -> Double
        findAverage xs sum counter
         | null xs = sum / counter
         | otherwise = findAverage (tail xs) (sum + (snd (head xs))) (counter + 1) 
        
cheaperAlternative :: StoreAvailability -> Int
cheaperAlternative xs = helper xs 0
    where
        helper :: StoreAvailability -> Int -> Int
        helper xs counter
         | null xs = counter 
         | checkForAlternative (head xs) (filter (\x -> (fst x) == (fst (head xs))) xs) = helper (filter (\x -> (fst x) /= (fst (head xs))) xs) (counter + 1)
         | otherwise = (helper (filter (\x -> (fst x) /= (fst (head xs))) xs) counter)
            where 
                checkForAlternative :: Product -> StoreAvailability -> Bool
                checkForAlternative x xs
                 | null xs = False
                 | (snd x) > (snd (head xs)) || (snd x) < (snd (head xs)) = True
                 | otherwise = checkForAlternative x (tail xs)

-- Задача 4. Нека е даден списък от точки в тримерно пространство, представен като списък от
-- наредени тройки. Да се напише на Haskell функция
-- minDistance :: [(Double,Double,Double)] -> Double, която намира най-малкото от
-- разстоянията между двойките точки от списъка.
-- Разстоянието d се дeфинира по следния начин: ако разглеждаме точките p1=(x1, y1, z1) и
-- p2=(x2, y2, z2), то d(p1, p2) = (x1-x2)*(x1-x2)+(y1-y2)*(y1-y2)+(z1-z2)*(z1-z2).
-- minDistance :: [(Double,Double,Double)] -> [[((Double,Double,Double),(Double,Double,Double))]]
-- minDistance ps = makeAllPairs (head ps) (tail ps)
--     where
--         makeAllPairs :: (Double,Double,Double)->[(Double,Double,Double)] ->[[((Double,Double,Double), (Double,Double,Double))]]
--         makeAllPairs p ps
--          | null ps = []
--          | otherwise = (makePairs p ps) : makeAllPairs (head ps) (tail ps)
--             where 
--                 makePairs :: (Double,Double,Double)->[(Double,Double,Double)] -> [((Double,Double,Double),(Double,Double,Double))]
--                 makePairs p ps 
--                  | null ps = []
--                  | otherwise = (p,(head ps)) : (makePairs (head ps) (tail ps))
minDistance :: [(Double,Double,Double)] -> Double
minDistance ps = let makeAllPairs = filter (\x -> length x == 2) ( subsequences ps)
                     distances = map (\x -> distance  (head x) (last x)) makeAllPairs
                     minDistance = minimum distances
                in minDistance
        where
            distance :: (Double,Double,Double)->(Double,Double,Double)-> Double
            distance (x1,y1,z1) (x2,y2,z2) = (x1-x2)*(x1-x2)+(y1-y2)*(y1-y2)+(z1-z2)*(z1-z2)

-- Задача 5. Да се дефинира функция
-- maximize :: (Ord a, Num a) => [(a -> a)] -> (a -> a), за която оценката на
-- обръщението maximize l, където l е непразен списък от едноместни числови функции, да е
-- едноместна числова функция на аргумент x, която дава стойността f(x) на тази фунция f от
-- списъка l, за която числото f(x) е най-голямо по абсолютна стойност.
-- Пример:
-- Ако fn = maximize [(\x -> x*x*x),(\x -> x+1)],
-- то fn 0.5 → 1.5, а fn (-2) → -8

argmax::(Ord a, Ord b) => (a->b) -> [a] -> a
argmax f xs = maximumBy (comparing f) xs

maximize :: (Ord a, Num a) => [(a -> a)] -> (a -> a)
maximize fn = (\x -> argmax abs (map (\f -> (f x) ) fn))

-- Задача 6. Функцията g е обратна на функцията f в дадено множество А, ако f . g = id в A и g . f =
-- id в A. Да се напише на езика Haskell функция
-- inverseFun :: (Int -> Int) -> (Int -> Int) -> Int -> Int -> Bool, която за
-- дадени целочислени функции f и g връща True точно когато g е обратна на f в даден
-- целочислен интервал [a, b].

inverseFun :: (Int -> Int) -> (Int -> Int) -> Int -> Int -> Bool
inverseFun f g a b = length (filter (\x -> x==False) (map (\x -> f (g x) == (id x) && g (f x) == (id x) ) [a..b])) == 0
-- inverseFun f g a b = and (map (\x -> f (g x) == (id x) && g (f x) == (id x) ) [a..b])

-- Да се напише на езика Haskell функция mirrorBst :: BTree а -> BTree а,
-- която получава двоично двоично дърво bt и го преобразува в “огледалното” му такова bt’
data BTree a = Empty | Node a (BTree a) (BTree a) deriving (Read, Show)

mirrorBst :: BTree a -> BTree a
mirrorBst Empty = Empty
mirrorBst (Node x lt rt) = (Node x (mirrorBst rt) (mirrorBst lt))

main = do
print $ reverseOrdSuff 37563 -- → 36
print $ reverseOrdSuff 32763 -- → 367
print $ reverseOrdSuff 32567 -- → 7
print $ reverseOrdSuff 32666 -- → 6

print $ sumUnique [[1,2,3,2],[-4,-4],[5]] -- → 9 (= 1+3+5)
print $ sumUnique [[2,2,2],[3,3,3],[4,4,4]] -- → 0
print $ sumUnique [[1,2,3],[4,5,6],[7,8,9]] -- → 45

print $ closestToAverage [("bread",1),("milk",2.5),("lamb",10),("cheese",5),("butter",2.3)] --  ”cheese”
print $ cheaperAlternative [("bread",1),("cheese",2.5),("bread",1),("cheese",5),("butter",2.3)] --  1

print $ minDistance [(1,1,1),(2,2,2),(3,3,3)]
print $ (maximize [(\x -> x*x*x),(\x -> x+1)]) 0.5 -- 1.5
print $ (maximize [(\x -> x*x*x),(\x -> x+1)]) (-2)

print $ inverseFun (\x -> x+1) (\x -> x-1) 5 10 -- → True
print $ inverseFun (\x -> x*x) (\x -> x^3) 0 1 -- → True
print $ inverseFun (\x -> x+1) (\x -> x+2) 0 1 -- → False
print $ [x| x <- [2..6] , even x] 

print $ mirrorBst (Node 4 (Node 5  (Node 6 Empty Empty) Empty) (Node 3 (Node 2 Empty Empty)(Node 1 Empty Empty)))