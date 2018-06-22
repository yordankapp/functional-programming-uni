
-- Използвайте fold(l/r), за да дефинирате следните функции:
-- а). sum xs, която връща сбора на елементите на xs.
-- б). product xs, която връща произведението на елементите на xs.
-- в). length xs, която връща броя на елементите на xs.
-- г). any p xs, която връща дали предикатът p e верен за поне един от елементите на xs.
-- д). all p xs, която връща дали предикатът p e верен за всеки от елементите на xs.
-- е). minimum xs, която връща най-малкия елемент на xs.
-- ж). maximum xs, която връща най-големия елемент на xs.
-- з). concat xss, която приема списък от списъци xss и ги конкатенира в един общ списък.

-- sumfold :: [Integer] -> Integer
-- --sumfold xs = foldl (\x acc -> x+acc) 0 xs
-- sumfold xs = foldl (+) 0 xs

-- productfold :: [Integer] -> Integer
-- --productfold xs = foldl (\x acc -> x*acc) 1 xs
-- productfold xs= foldl (*) 1 xs

-- lengthfold :: [Int] -> Int
-- lengthfold xs = foldl (\res _ -> (res + 1)) 0 xs

-- anyfold :: (a->Bool) -> [a] -> Bool
-- anyfold p xs = foldl (\res x  -> res || p x ) False xs

-- allfold ::  (a->Bool) -> [a] -> Bool
-- allfold p xs = foldl (\res x  -> res && p x ) True xs

-- minimumfold :: (Ord a) => [a] -> a
-- minimumfold xs = foldl1 min xs

-- maximumfold ::(Ord a) => [a] -> a
-- maximumfold xs = foldl1 max xs

-- concatfold :: [[a]] ->[a]
-- concatfold xss = foldr1 (++) xss

-- -- Използвайте fold(l/r), за да дефинирате функцията reverse xs, която приема списък xs и обръща елементите му.
-- reversefold :: [Integer] -> [Integer]
-- reversefold xs = foldl (flip (:) ) [] xs
-- -- reversefold xs = foldl (\ res x -> x:res) [] xs

-- двоично дърво 
-- NilT = Empty
data BTree = NilT | Node Int BTree BTree deriving Show
bt :: BTree
bt = NilT 

bt2 :: BTree
bt2 = Node 5 (Node 4 NilT NilT) (Node 7 NilT (Node 9 NilT NilT))

-- да се преброят възлите 
btCount:: BTree -> Int 
btCount NilT = 0
btCount (Node _ lt rt) = 1 + btCount lt + btCount rt

-- да се изчисли сумата
btSum :: BTree -> Int
btSum NilT = 0
btSum (Node x lt rt) = x + btSum lt + btSum rt
-- btSum (Node _ lt rt) = x + btCount lt + btCount rt

-- добавяне на връх в дървото
btInsert :: Int -> BTree -> BTree
btInsert x NilT = Node x NilT NilT
btInsert x bt@(Node y lt rt) 
  | x < y = Node y (btInsert x lt) rt 
  | x > y = Node y lt (btInsert x rt) 
  | otherwise = bt 

-- обхождане  
btOrder :: BTree -> [Int]
btOrder NilT = []
btOrder (Node x lt rt) = btOrder lt ++ [x] ++ btOrder rt 

-- sort
btSort :: [Int] -> [Int] 
btSort xs = btOrder (foldr btInsert NilT xs) 

main = do
    -- print $ sumfold [1,5,2]
    -- print $ productfold [1,5,2] 
    -- print $ lengthfold [1,5,2] 
    -- print $ anyfold even [1,5,2] 
    -- print $ allfold even [1,5,2]
    -- print $ minimumfold [1,5,2] 
    -- print $ maximumfold  [1,5,2] 
    -- print $ concatfold  [[1],[5,2],[0]]
    -- print $ reversefold [1,5,2]
    
    print $ bt
    print $ bt2
    print $ (btCount bt2)
    print $ (btSum bt2)
    print $ (btInsert 6 bt2)
    print $ (btOrder bt2)
    print $ (btSort (btOrder bt2))