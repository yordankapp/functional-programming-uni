
import Data.List -- за group 

-- от 1вото домашно на Racket
-- Задача 4. Да се напише функция maximize, която получава непразен списък от
-- едноместни числови функции и връща нова едноместна числова функция на
-- аргумент x, която връща стойността f(x) на тази фунция f от списъка, за която
-- числото f(x) е най-голямо по абсолютна стойност.

-- maximize :: [(a -> a )] -> (a -> a)
-- maximize fs = (\ x -> helper (tail fs) (head fs) x) -- \ lambda
--     where 
--         helper :: [(a -> a)] -> (a -> a) -> a -> a
--         helper fs res x 
--          | null fs = res x
--          | abs ((head fs) x) > abs (res x) = helper (tail fs) (head fs) x
--          | otherwise = helper (tail fs) res x
 
compose :: (a -> b) -> (c -> a) -> (c -> b)
compose f g = \x -> f (g x)

maximize :: [(Int -> Int)] -> (Int -> Int)
maximize fs x = maximum (map abs afs)
    where afs = map (\f -> f x) fs

--зад 2 oт домашното на Racket
encode :: [Char] -> [(Char,Int)]
encode fs = map (\xs -> (head xs, length xs)) (group fs)

encode2 :: [Char] -> [(Char,Int)]
encode2 fs = [(head xs, length xs) | xs <- (group fs) ]

main = do
    
     print $ ((maximize [\ x -> x - 1 , \ x -> x * 2]) 5)
     print $ maximum [(1,2), (0,3), (5,10), (5,4)]
     print $ (group [1,2,2,2,3,3,4]) -- [[1],[2,2,2],[3,3],[4]]
