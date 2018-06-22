
import  Data.List -- it has a bunch of useful functions for working with lists

--A Haskell module is a collection of related functions, types and typeclasses. 
--A Haskell program is a collection of modules where the main module loads up the 
--other modules and then uses the functions defined in them to do something. 

-- how many unique elements a list has
--nub is a function defined in Data.List that takes a list and weeds out duplicate elements
numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub


main = do
    print $ numUniques [1,2,3,3,4]

    -- You can also put the functions of modules into the global namespace 
    --when using GHCI. If you're in GHCI and you want to be able to call the 
    --functions exported by Data.List, do this:
    -- ghci> :m + Data.List  
    -- If we want to load up the names from several modules inside GHCI, 
    --we don't have to do :m + several times, we can just load up several modules at once.
    -- ghci> :m + Data.List Data.Map Data.Set 

    --If you just need a couple of functions from a module, you can selectively import just those functions. 
    --import Data.List (nub, sort)  

    --You can also choose to import all of the functions of a module except a few select ones.
    --import Data.List hiding (nub) 

    --Another way of dealing with name clashes is to do qualified imports. 
    --import qualified Data.Map 

    -- we can rename the qualified import to something shorter:
    --import qualified Data.Map as M  
    --Now, to reference Data.Map's filter function, we just use M.filter.

    --intersperse takes an element and a list and then puts that 
    --element in between each pair of elements in the list. 

    print $ intersperse '.' "Monkey"
    print $ intersperse 0 [1,2,3,4]

    --intercalate takes a list of lists and a list. 
    --It then inserts that list in between all those lists and then flattens the result.
    print $ intercalate " " ["hey","there","guys"]
    print $ intercalate [0,0] [[1,2],[3,4],[5,6]]

    print $ transpose [[1,2,3],[4,5,6],[7,8,9]]

    -- 3x^2 + 5x + 9, 10x^3 + 9 and 8x^3 + 5x^2 + x - 1 and we want to add them together
    print $ map sum $ transpose [[0,3,5,9],[10,0,0,9],[8,5,1,-1]] 

    -- foldl' and foldl1' are stricter versions of their respective lazy incarnations.
    -- if you ever get stack overflow errors when doing lazy folds, try switching to their strict versions

    print $ concat ["banana","omlet","hey"]
    print $ concat [[1,2],[3,4],[5,6]]

    --concatMap is the same as first mapping a function to a list and then concatenating the list with concat
    print $ concatMap (replicate 4) [1..3]

    print $ and $ map (>4) [4,10,80,5,2]
    print $ or $ map (>4) [4,10,80,5,2]

    --any and all take a predicate and then check if any or all the elements in a list satisfy 
    --the predicate, respectively. Usually we use these two functions instead of mapping over
    -- a list and then doing and or or.
    print $ any (==4) [2,1,3,4,5,4]
    print $ all (>4) [5,8,10]
    print $ any (`elem` ['A'..'Z'] ) "Hello"

    --iterate takes a function and a starting value. It applies the function to the starting value, 
    --then it applies that function to the result, then it applies the function to that result again,
    -- etc. It returns all the results in the form of an infinite list.
    print $ take 10 $ iterate (*2) 1
    print $ take 3 $ iterate (++ "hey") "abc"

    --splitAt takes a number and a list. It then splits the list at that
    -- many elements, returning the resulting two lists in a tuple
    print $ splitAt 3 "heyman"
    print $ splitAt 100 "heyman"
    print $ splitAt (-3) "heyman"
    print $ let (a,b) = splitAt 3 "heyman" in b ++ a

    --takeWhile is a really useful little function. It takes elements from a list
    -- while the predicate holds and then when an element is encountered that doesn't
    -- satisfy the predicate, it's cut off.
    print $ takeWhile (>3) [6,5,4,3,2,1]
    print $ takeWhile (/=' ') "This is an omlet"
    print $ sum $ takeWhile (<10000) $ map (^3) [1..] 
    print $ dropWhile (>3) [6,5,4,3,2,1]
    print $ dropWhile (/=' ') "This is an omlet"

    --span is kind of like takeWhile, only it returns a pair of lists.
    -- The first list contains everything the resulting list from takeWhile
    -- would contain if it were called with the same predicate and the same list. 
    --The second list contains the part of the list that would have been dropped.
    print $ span (/=4) [1,2,3,4,5,6,7]
    print $ let (fw, rest) = span (/=' ') "This is a sentence" in "First word:" ++ fw ++ ", the rest:" ++ rest 

    --break breaks it when the predicate is first true
    -- break, the second list in the result will start with the first element that satisfies the predicate.
    print $ break (==4) [1,2,3,4,5,6,7]

    --sort - The type of the elements in the list has to be part of the Ord typeclass
    print $ sort [8,5,2,5,6,3]
    print $ sort "omlet hey banana ey"

    --group takes a list and groups adjacent elements into sublists if they are equal
    print $ group [1,1,2,3,3,4,5,5,5,2]

    --how many times each element appears in the list.
    print $  map (\l@(x:xs) -> (x,length l)) . group . sort $ [1,1,2,3,3,4,5,5,5,2]

    print $ inits "hello"
    print $ tails "hello"

    --isInfixOf searches for a sublist within a list and returns True if the sublist
    -- we're looking for is somewhere inside the target list
    print $ isInfixOf "cat" "Do you have a cat?"

    --isPrefixOf and isSuffixOf search for a sublist at the beginning and at the end of a list, respectively.
    print $ isPrefixOf "hello" "hello world"
    print $ isSuffixOf "world" "hello world"

    --elem and notElem check if an element is or isn't inside a list
    
    --partition takes a list and a predicate and returns a pair of lists. 
    --The first list in the result contains all the elements that satisfy the predicate, 
    --the second contains all the ones that don't
    --it goes through the whole list and splits it up according to the predicate
    print $ partition (`elem` ['A'..'Z']) "HEwoYrld"
    
    --find takes a list and a predicate and returns the first element that 
    --satisfies the predicate. But it returns that element wrapped in a Maybe value.
    -- a Maybe value can either be Just something or Nothing
    -- Maybe value can be either no elements or a single element

    print $ find (>4) [1,2,3,4,5,6] 
    print $ find (>9) [1,2,3,4,5,6] 

    --elemIndex is kind of like elem, only it doesn't return a boolean value. 
    --It maybe returns the index of the element we're looking for. 
    --If that element isn't in our list, it returns a Nothing.
    print $ 4 `elemIndex` [1,2,3,4,5,6] 
    print $ 10 `elemIndex` [1,2,3,4,5,6] 

    --elemIndices is like elemIndex, only it returns a list of indices, 
    --in case the element we're looking for crops up in our list several times. 
    print $ ' ' `elemIndices` "Where are the spaces"

    --findIndex is like find, but it maybe returns the index of the first element
    -- that satisfies the predicate. 
    --findIndices returns the indices of all elements that satisfy the predicate in the form of a list.
    print $ findIndex (==4) [1,4,2,4,3]
    print $ findIndices (`elem` ['A'..'Z']) "Where Are The Caps?"

    --lines is a useful function when dealing with files or input from somewhere. 
    --It takes a string and returns every line of that string in a separate list.
    print $ lines "first line\nsecond line\nthird line" 
    print $ unlines ["first line", "second line", "third line"]  

    --words and unwords are for splitting a line of text into words or joining a list of words into a text
    print $ words "hey these are the words in this sentence" 
    print $ words "hey these           are    the words in this\nsentence" 
    print $ unwords ["hey","there","mate"]  

    --delete takes an element and a list and deletes the first occurence of that element in the list.
    print $ delete 'h' "hey there ghang!"  

    -- \\ is the list difference function. It acts like a set difference, basically. 
    --For every element in the right-hand list, it removes a matching element in the left one.
    print $ [1..10] \\ [2,5,9]
    print $ "Hello big world!" \\ "big"

    --union also acts like a function on sets. It returns the union of two lists.
    -- It pretty much goes over every element in the second list and appends it to the first one
    -- if it isn't already in yet. duplicates are removed from the second list!
    print $ "hey man" `union` "man what's up"
    print $ [1..7] `union` [5..10]
    print $ [1..7] `intersect` [5..10]

    --insert takes an element and a list of elements that can be sorted and inserts it into 
    --the last position where it's still less than or equal to the next element. In other words, 
    --insert will start at the beginning of the list and then keep going until it finds an element 
    --that's equal to or greater than the element that we're inserting and it will insert it just 
    --before the element.
    print $ insert 4 [1,8,5,9,2,1]
    print $ insert 'g' $ ['a'..'f'] ++ ['h'.. 'z']
    print $ insert 3 [1,2,4,3,2,1] 
    
    
    