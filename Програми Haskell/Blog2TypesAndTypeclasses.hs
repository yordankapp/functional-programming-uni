
--The type of every expression is known at compile time, which leads to safer code.
-- ':t' command which, followed by any valid expression, tells us its type

   :t 'a'  
  -- 'a' :: Char  
   :t True  
  -- True :: Bool  
   :t "HELLO!"  
   --"HELLO!" :: [Char]  
   :t (True, 'a')  
  -- (True, 'a') :: (Bool, Char)  
   :t 4 == 5  
  -- 4 == 5 :: Bool 
  addThree :: Integer -> Integer -> Integer
  addThree x y z = x + y + z

  --Int stands for integer. It's used for whole numbers
  --Integer stands for also integer. It can be used to represent really big numbers. 
  --Float is a real floating point with single precision.
  --Double is a real floating point with double the precision
  --Bool is a boolean type. It can have only two values: True and False.
  --Char represents a character. It's denoted by single quotes. A list of characters is a string.

  --Tuples are types but they are dependent on their length as well as the types of their components, 
  --so there is theoretically an infinite number of tuple types. 
  --Note that the empty tuple () is also a type which can only have a single value: ()

  :t head  
  head :: [a] -> a  
  --it's actually a type variable. That means that a can be of any type.

  --Functions that have type variables are called polymorphic functions. 

  :t (==)  
  (==) :: (Eq a) => a -> a -> Bool 
  -- Everything before the => symbol is called a class constraint. 
  --We can read the previous type declaration like this: the equality 
  --function takes any two values that are of the same type and returns a Bool. 
  --The type of those two values must be a member of the Eq class

  --Some basic typeclasses:
  --Eq is used for types that support equality testing.
  5 == 5  
  True  
   5 /= 5  
  False  
   'a' == 'a'  
  True  
   "Ho Ho" == "Ho Ho"  
  True  
  3.432 == 3.432  
  True
 -- Ord is for types that have an ordering.
  :t (>)  
  (>) :: (Ord a) => a -> a -> Bool 
  --Ordering is a type that can be GT, LT or EQ, meaning greater than, lesser than and equal, respectively.
  To be a member of Ord, a type must first have membership in the prestigious and exclusive Eq club.
  
  "Abrakadabra" < "Zebra"  
  True  
   "Abrakadabra" `compare` "Zebra"  
  LT  
   5 >= 2  
  True  
   5 `compare` 3  
  GT   

  --Members of Show can be presented as strings. All types covered so far except for functions are 
  --a part of Show. The most used function that deals with the Show typeclass is show. 
  --It takes a value whose type is a member of Show and presents it to us as a string.
  
  ghci> show 3  
  "3"  
  ghci> show 5.334  
  "5.334"  
  ghci> show True  
  "True"  

  --Read is sort of the opposite typeclass of Show. 
  --The read function takes a string and returns a type which is a member of Read.
  
  ghci> read "True" || False  
  True  
  ghci> read "8.2" + 3.8  
  12.0  
  ghci> read "5" - 2  
  3  
  ghci> read "[1,2,3,4]" ++ [3]  
  [1,2,3,4,3]  
  --It returns a type that's part of Read but if we don't try to use it in some way later, 
  --it has no way of knowing which type. 

  --Type annotations are a way of explicitly saying what the type of an expression should be. 
  --We do that by adding :: at the end of the expression and then specifying a type. 
  ghci> read "5" :: Int  
  5  
  ghci> read "5" :: Float  
  5.0  
  ghci> (read "5" :: Float) * 4  
  20.0  
  ghci> read "[1,2,3,4]" :: [Int]  
  [1,2,3,4]  
  ghci> read "(3, 'a')" :: (Int, Char)  
  (3, 'a')  

  --Enum members are sequentially ordered types — they can be enumerated. 
  --The main advantage of the Enum typeclass is that we can use its types in list ranges. 

  --Bounded members have an upper and a lower bound.
  --Num is a numeric typeclass. Its members have the property of being able to act like numbers. 
  --To join Num, a type must already be friends with Show and Eq.
  --Integral is also a numeric typeclass. Integral includes only integral (whole) numbers. 
  --Floating includes only floating point numbers, so Float and Double.
  
  --A very useful function for dealing with numbers is fromIntegral. 
  --it takes an integral number and turns it into a more general number. 