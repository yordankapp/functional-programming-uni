
--the first parameter is a function that takes something and returns that same thing. 
--The second parameter is something of that type also and the return value is also of the same type.
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- It takes a function and two lists as parameters and then joins the two lists 
-- by applying the function between corresponding elements.
--The first parameter is a function that takes two things and produces a third thing. 
--They don't have to be of the same type, but they can. The second and third parameter are lists.
-- The result is also a list. The first has to be a list of a's, because the joining function 
--takes a's as its first argument. The second has to be a list of b's, because the second parameter 
--of the joining function is of type b. The result is a list of c's. If the type declaration of
-- a function says it accepts an a -> b -> c function as a parameter, it will also accept 
--an a -> a -> a function, but not the other way around! Remember that when you're making 
--functions, especially higher order ones, and you're unsure of the type, you can just try 
--omitting the type declaration and then checking what Haskell infers it to be by using :t.
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y: zipWith' f xs ys

--Flip simply takes a function and returns a function that is 
--like our original function, only the first two arguments are flipped
flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
    where g x y= f y x

flip2' :: (a -> b -> c) -> b -> a -> c
flip2' f y x = f x y

--map takes a function and a list and applies that function to 
--every element in the list, producing a new list. 
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

--filter is a function that takes a predicate and a list 
--and then returns the list of elements that satisfy the predicate. 
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
    | p x  = x : filter' p xs
    | otherwise = filter' p xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallersort = quicksort (filter (<=x) xs)
        biggersort =  quicksort (filter (>x) xs)
    in smallersort ++ [x] ++ biggersort

flip3' :: (a -> b -> c) -> b -> a -> c  
flip3' f = \ x y -> f y x 

--A fold takes a binary function, a starting value (I like to call it the accumulator)
-- and a list to fold up. The binary function itself takes two parameters. 
--The binary function is called with the accumulator and the first (or last) element and produces 
--a new accumulator. Then, the binary function is called again with the new accumulator and the 
--now new first (or last) element, and so on. 

--foldl function - It folds the list up from the left side. 
--The binary function is applied between the starting value and the head of the list. 
--That produces a new accumulator value and the binary function is called with that value 
--and the next element, etc.
--if we call a fold on an empty list, the result will just be the starting value
sumfoldl :: (Num a) => [a] -> a
sumfoldl xs = foldl (\ accumulator x -> accumulator + x) 0 xs

sumfoldl':: (Num a) => [a] -> a
sumfoldl' = foldl (+) 0

elem':: (Ord a) => a -> [a] -> Bool
elem' x xs= foldl (\ accumulator y -> if x == y then True else accumulator) False xs

--The right fold- the accumulator eats up the values from the right. 
--left fold's binary function ( \acc x -> ...)
-- right fold's binary function (\x acc -> ...)
--The accumulator value of a fold can be of any type
mapfoldr :: (a -> b) -> [a] -> [b]
mapfoldr f xs = foldr (\ x accumulator -> f x : accumulator) [] xs 

--the ++ function is much more expensive than : , 
--so we usually use right folds when we're building up new lists from a list
--that right folds work on infinite lists

--The foldl1 and foldr1 functions work much like foldl and foldr, 
--only you don't need to provide them with an explicit starting value. 
--They assume the first (or last) element of the list to be the starting 
--value and then start the fold with the element next to it

maximumfoldr1::(Ord a) => [a] -> a
maximumfoldr1 = foldr1 (\ x accumulator -> if x > accumulator then x else accumulator)

reversefoldl1 :: [a] -> [a]
reversefoldl1 = foldl (\ accumulator x -> x:accumulator) []

productfoldl1 :: (Num a) => [a] -> a
productfoldl1 = foldl1 (*)

-- filterfoldr :: ( a -> Bool) -> [a] -> [a]
-- filterfoldr p = foldr (\ x accumulator -> if p x then x else accumulator) []

headfoldr1 :: [a] -> a
headfoldr1 = foldr1 (\x _ -> x)

lastfoldl1 :: [a] -> a
lastfoldl1 = foldl1 (\ _ x -> x)

sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1

main = do 
    print $ applyTwice (+ 3) 10
    print $ applyTwice (++ " HAHA") "HEY"  
    print $ applyTwice ("HAHA " ++) "HEY"   
    print $ applyTwice (3:) [1]  

    print $ zipWith' (+) [4,2,5,6] [2,6,2,3] 
    print $ zipWith' max [6,3,2,1] [7,3,1,5]  
    print $ zipWith'(++) ["foo ", "bar ", "baz "] ["fighters", "hoppers", "aldrin"] 
    print $ zipWith' (*) (replicate 5 2) [1..] 
    print $ zipWith' (zipWith' (*)) [[1,2,3],[3,5,6],[2,3,4]] [[3,2,2],[3,4,5],[5,4,3]] 
    
    print $ flip2' zip [1,2,3,4,5] "hello" 
    print $ zipWith (flip' div) [2,2..] [10,8,6,4,2] 

    print $ map' (+3) [1,5,3,1,6]
    print $ map (*2) [1,5,2]

    print $ filter' (>3) [1,5,3,2,1,6,4,3,2,1] 
    print $ filter' (==3) [1,2,3,4,5] 
    print $ filter' even [1..10] 
    print $ filter' (`elem` ['a'..'z']) "u LaUgH aT mE BeCaUsE I aM diFfeRent"  
    print $ filter (>2) [1,3,0,5,10]
    
    print $ quicksort [1,5,3,2,1,6,4,3,2,1] 

    --Let's find the largest number under 100,000 that's divisible by 3829
   
    let largestDivisible = (head (filter (\x -> (x `mod` 3829) == 0 ) [100000, 99999 ..]))
    print $ largestDivisible

    -- takeWhile function. It takes a predicate and a list and then goes from the
    -- beginning of the list and returns its elements while the predicate holds true. 
    --Once an element is found for which the predicate doesn't hold, it stops. 
    --If we wanted to get the first word of the string "elephants know how to party",
    -- we could do takeWhile (/=' ') "elephants know how to party" and it would return "elephants".

    --find the sum of all odd squares that are smaller than 10,000
    print $ sum (takeWhile (< 10000) (filter odd (map (^2) [1..])))
    print $ sum (takeWhile (< 10000) [x^2 | x <- [1..], odd (x^2) ])

    --length returns an Int instead of a Num a for historical reasons

    --Using map, we can also do stuff like map (*) [0..], if not for any other reason than 
    --to illustrate how currying works and how (partially applied) functions are real values that
    -- you can pass around to other functions or put into lists (you just can't turn them to strings). 
    --So far, we've only mapped functions that take one parameter over lists, like map (*2) [0..] 
    --to get a list of type (Num a) => [a], but we can also do map (*) [0..] without a problem.
    -- What happens here is that the number in the list is applied to the function *, which has 
    --a type of (Num a) => a -> a -> a. Applying only one parameter to a function that takes two 
    --parameters returns a function that takes one parameter. If we map * over the list [0..], 
    --we get back a list of functions that only take one parameter, so (Num a) => [a -> a]. 
    --map (*) [0..] produces a list like the one we'd get by writing [(0*),(1*),(2*),(3*),(4*),(5*)...
    let listOfFuns = map (*) [0..]
    print $ ((listOfFuns !! 4 ) 5)

    --Lambdas are basically anonymous functions that are used because we need some functions only once.
    print $ zipWith (\a b -> (a*30 +3) / b ) [5,4,3,2,1] [1,2,3,4,5]
    print $ map (\(a, b) -> a+b) [(1,2), (3,5), (6,3), (2,6), (2,5)]

    print $ sumfoldl [3,5,2,1]
    print $ sumfoldl' [3,5,2,1]

    print $ elem' 3 [3,5,2,1]
    print $ elem' 12 [5,11,20,66]

    print $ mapfoldr (+3) [3,5,2,1]

    print $ maximumfoldr1 [2,5,10,80,6,3,0]
    print $ reversefoldl1 [1,2,3,4]
    print $ productfoldl1 [1,5,6]
   -- print $ filterfoldr (>10) [1,20,55,6,8,64]
    print $ headfoldr1 [5,2,3]
    print $ lastfoldl1 [5,2,3]

   --scanl and scanr are like foldl and foldr, only they report all the intermediate accumulator 
   --states in the form of a list. There are also scanl1 and scanr1, which are analogous to foldl1 and foldr1.
    print $ scanl (+) 0 [1,2,3,4]
    print $ scanr (+) 0 [1,2,3,4]
    print $ scanl1 (\ accumulator x -> if x > accumulator then x else accumulator) [3,4,5,3,7,9,2,1]
    print $ scanl (flip (:)) [] [3,2,1]

    --When using a scanl, the final result will be in the last element of 
    --the resulting list while a scanr will place the result in the head.
    --Scans are used to monitor the progression of a function that can be implemented as a fold.

    print $ sqrtSums
    print $ sum (map sqrt [1..131])

    --Function application with $
    --($) :: (a -> b) -> a -> b  
    --f $ x = f x  
    --Function application with a space is left-associative 
    --(so f a b c is the same as ((f a) b) c)), 
    --function application with $ is right-associative.

    --When a $ is encountered, the expression on its right is 
    --applied as the parameter to the function on its left.

    print $ sqrt $ 3 + 4 + 5
    -- $ means that function application can be treated just like another function
    print $ map ($ 3) [(4+), (10*), (^2), sqrt]

    -- . function, which is defined like so:
    -- (.) :: (b -> c) -> (a -> b) -> a -> c  
    -- f . g = \x -> f (g x) 
    
    -- f must take as its parameter a value that has the same type as g's return value. 
    --So the resulting function takes a parameter of the same type that g takes and 
    --returns a value of the same type that f returns. 
    print $ map (negate . abs) [5,-3,-6,7,-3,2,-19,24] 

    --Function composition is right-associative, so we can compose many functions at a time. 
    print $ map ( negate . sum . tail) [[1..5],[3..6],[1..7]] 

    -- If you want to rewrite an expression with a lot of parentheses by using function composition, 
    --you can start by putting the last parameter of the innermost function after a $ and then just 
    --composing all the other function calls, writing them without their last parameter and putting 
    --dots between them.
    



