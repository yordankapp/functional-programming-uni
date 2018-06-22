

doubleMe :: Integer -> Integer
doubleMe x = x + x 

doubleUs :: Integer -> Integer -> Integer
doubleUs x y = doubleMe x + doubleMe y

boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]     

--'_' means that we don't care what we'll draw from the list 
length' xs = sum [1 |  _  <- xs]

-- because strings are lists, we can use list comprehensions to process and produce strings
removeNonUppercase st = [c| c <- st , c `elem` ['A'..'Z'] ]


main = do
    print $ 5 + 3 
    print $ 5 * 10
    print $ succ 8 -- 9
    print $ succ 52 -- 53
    print $ min 5 3 
    print $ max 5 3
    print $ doubleMe 3
    print $ doubleUs 3 5
    let lostNumbers = [4,18,32]
    print $ lostNumbers
    print $ [1,2,3] ++ [4,5,6]
    print $ "hello" ++ " " ++ "world"
    print $ ['h','e'] ++ ['l','l', 'o']
    print $ 'A' : " SMALL CAT"
    print $ 5 : [1,2,3] --putting something at the beginning of a list using the ':' operator 
    print $ 1:2:3:[] -- [1,2,3]
    --If you want to get an element out 
    --of a list by index, use '!!'. 
    --The indices start at 0.
    print $ "Steve Buscemi" !! 6
    print $ [1,2,3] !! 1
    let b = [[1,2],[3],[4,5]]
    print $ b ++ [[6],[7,8]]
    print $ [9,10] : b
    print $ b !! 2
    --Lists can be compared if the stuff they contain can be compared. 
    --When using <, <=, > and >= to compare lists, they are compared in 
    --lexicographical order. First the heads are compared. 
    --If they are equal then the second elements are compared, etc.
    print $ [8,10,12] > [5,7,6]
    print $ [3,2,1] > [2,10,100]
    print $ [1,2,10,4] < [1,2,100,3]
    print $ head [5,6,7,8,9,10] -- 5
    print $ tail [5,6,7,8,9,10] -- [6,7,8,9,10]
    print $ last [5,6,7,8,9,10] -- 10
    print $ init [5,6,7,8,9,10] -- [5,6,7,8,9]
    print $ length [1,2,3,4]
    print $ null []
    print $ null [1,2]
    print $ reverse [1,2,3]
    --take takes number and a list
    --It extracts that many elements from the beginning of the list
    print $ take 3 [1,2,3,4,5,6]
    --drop works in a similar way, only it drops the number of elements 
    --from the beginning of a list
    print $ drop 3 [1,2,3,4,5,6]
    print $ drop 100 [1,2,3]
    print $ maximum [10,5,1,6,7]
    print $ minimum [10,5,1,6,7]
    print $ sum [1,2,3]
    print $ product [1,2,3]
    --elem takes a thing and a list of things and tells 
    --us if that thing is an element of the list
    print $ 4 `elem` [3,4,5,6,4]
    print $ [1..5]
    print $ ['a'..'h']
    print $ ['A'..'M']
    --Ranges are cool because you can also specify a step
    print $ [1,3..10]
    print $ [20,17..2]
    print $ take 6 [1,3..]
    --cycle takes a list and cycles it into an infinite list.
    -- If you just try to display the result, it will go on forever 
    --so you have to slice it off somewhere.
    print $ take 10 (cycle [1,2,3])
    print $ take 12 (cycle "LOL ")
    --repeat takes an element and produces an infinite 
    --list of just that element. It's like cycling a list with only one element.
    print $ take 4 (repeat 5)
    --replicate function if you want some number of the same element in a list
    print $ replicate 3 10
    print $ [x*2| x <- [1..10]]
    print $ [x | x <- [50..100], x `mod` 7 == 3 ]
      
    print $ boomBangs [7..13]
    print $ [x | x <- [10..20], x /= 13 , x /= 15 , x /= 19]

    --get the products of all the possible combinations between numbers in those lists
    print $ [ x*y | x <- [2,5,10] , y <- [8,10,11]]
    print $ [ x*y | x <- [2,5,10] , y <- [8,10,11] , x*y > 50]

    let nouns = ["hobo","frog","pope"]
    let adjectives = ["lazy","grouchy","scheming"]
    print $ [adjective ++ " " ++ noun | adjective <- adjectives , noun <- nouns]

    print $ length' [1,2,3]
    print $ removeNonUppercase "Hahahaha! Ahahhaa!"

    --Nested list comprehensions are also possible if you're operating on lists that contain lists.
    let xxs = [[1,3,5,2,3,1,2,4,5],[1,2,3,4,5,6,7,8,9],[1,2,4,2,1,6,3,1,3,2,3,6]] 
    print $ [[ x| x <- xs , even x]| xs <- xxs]

    --In some ways, tuples are like lists — they are a way to store several values into a single value.
    -- tuples can also contain lists
    --Use tuples when you know in advance how many components some piece of data should have
    --Like lists, tuples can be compared with each other if their components can be compared. 
    --Only you can't compare two tuples of different sizes, whereas you can compare two lists of different sizes. 
    
    --Two useful functions that operate on pairs.They won't work on triples
    print $ fst (8,2)
    print $ snd (8,2)

    -- function that produces a list of pairs: zip. It takes two lists and then zips 
    --them together into one list by joining the matching elements into pairs. 
    print $ zip [1,2,3,4] [5,6,7,8]
    print $ zip [1,2,3] ["one","two","three"]
    --The longer list simply gets cut off to match the length of the shorter one. 
    print $ zip [1,2,3,4,5,6,7,8,9] ["apple", "banana","mango"]
    print $ zip [1..] ["red" , "green", "blue"] 

    let triangles = [(a,b,c) | c <- [1..10], b <- [1..10], a <- [1..10]]
    let rightTriangles = [(a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2]
    let rightTriangles' = [(a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a+b+c==24]    
    print $ rightTriangles'





    

