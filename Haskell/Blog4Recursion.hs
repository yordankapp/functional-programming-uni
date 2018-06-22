

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of an empty list"
maximum' [x] = x
maximum' (x:xs) 
    | x > maxTail = x
    | otherwise = maxTail 
    where maxTail = maximum' xs


maximum2' :: (Ord a) => [a] -> a
maximum2' [] = error "maximum of an empty list"
maximum2' [x] = x
maximum2' (x:xs) = max x (maximum' xs)

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
    | n <= 0 = []
    | otherwise = x:replicate' (n-1) x

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs 

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x: repeat' x

zip' :: [a] -> [b] -> [(a,b)]
zip' _  [] = []
zip' []  _ = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
    | a == x  = True
    | otherwise = elem' a xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallersorted = quicksort [a | a <- xs , a <= x]
        bigsorted = quicksort [a | a <- xs , a > x]
    in smallersorted ++ [x] ++ bigsorted

main = do
    print $ maximum' [1,45,10]
    print $ maximum2' [1,45,10]
    print $ replicate' 3 5
    print $ take' 3 [1,5,9,10,12,18]
    print $ reverse' [5,1,2]
    print $ take' 5 (repeat' 3)
    print $ zip' [1,2,3] [4,5]
    print $ elem' 2 [1,5,2,3,5,2]
    print $ elem' 2 [5,3,1]
    print $ quicksort [10,2,5,3,1,6,7,4,2,3,4,8,9] 
    print $ quicksort "the quick brown fox jumps over the lazy dog"  