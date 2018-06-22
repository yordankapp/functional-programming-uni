
--pattern matching
addVectors :: (Num a) => (a,a) -> (a,a) -> (a,a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

lenght' :: (Num b) => [a] -> b
lenght' [] = 0
lenght' ( _ :xs) = 1 + lenght' xs -- the length is equal to 1 plus the length of the tail

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

--patterns
--Those are a handy way of breaking something up according to a pattern 
--and binding it to names whilst still keeping a reference to the whole thing. 
--You do that by putting a name and an @ in front of a pattern. 
capital :: String -> String
capital "" = "Empty string!"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x] 

--Guards are a way of testing whether some property of a value (or several of them) are true or false. 
--a lot more readable when you have several conditions 
--If no suitable guards or patterns are found, an error is thrown.
--We put the keyword where after the guards and then we define several names or functions. 
--The names we define in the where section of a function are only visible to that function

--let <bindings> in <expression>
--The names that you define in the let part are accessible to the expression after the in part
cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^2
    in sideArea + 2 * topArea

calcBmis :: (RealFloat a) => [(a,a)] -> [a]
calcBmis xs = [bmi | (w,h) <- xs , let bmi = w / h ^2]

--case expression of pattern -> result  
--                  pattern -> result  
--                  pattern -> result  
--                  ...  

describeList :: [a] -> String
describeList xs = "This list is " ++ case xs of [] -> "empty"
                                                [x] -> "sigleton list"
                                                xs -> "longer list"

main = do

    -- pattern match in list comprehensions
    let xs = [(1,3), (4,3), (2,4), (5,3), (5,6), (3,1)]
    print $ [a+b | (a,b) <- xs]

    print $ lenght' [1,2]
    print $ lenght' "banana"

    print $ sum' [4,5,6]
    print $ capital "Dracula"

    print $ [ if 5 > 3 then "Woo" else "Boo", if 'a' > 'b' then "Foo" else "Bar"]
    print $ 4 * (let a = 9 in a + 1) + 2
    print $ [let square x = x * x in (square 5 , square 3 , square 2)]
    print $ (let a = 100; b = 200; c = 300 in a*b*c, let foo="Hey "; bar= "there!" in foo ++ bar)

    let zoot x y z = x * y + z
    print $ zoot 3 9 2
    
    let boot x y z = x * y + z in boot 3 9 2
    print $ boot

    print $ describeList [123]

  