{- C++ enum -}
-- Сезон
data Season = Spring | Summer | Autumn | Winter deriving (Read, Show)

-- Месеци
data Month = January | February | March | April | May | June | July | August | September | October | Novemer | December deriving (Read, Show)

{- C++ struct or (tagged) union -}
data Person = Person String Int
-- record syntax:
-- data Person = Person { name :: String, age :: Int }
--     deriving (Read, Show)
data Book = Book { bookTitle :: String, bookPublished :: Int, bookSales :: Int } deriving (Read, Show)

data Shape = Circle Double | Square Double  deriving (Read, Show)

{- C++ container classes -}
-- Списък от произволни елементи (от тип a)
data List a = Nil | Cons a (List a) deriving (Read, Show)

-- Tree a, описващ произволно двоично дърво, чийто стойности са от тип a.
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Read, Show)
-- data Tree  = Empty | Node Int (Tree ) (Tree ) deriving (Read, Show)


{- Примери -}
-- Дълбочина на двоично дърво:
treeDepth :: (Num b, Ord b) => Tree a -> b
treeDepth Empty = 0
treeDepth (Node _ left right) = 1 + max (treeDepth left) (treeDepth right)

-- Брой на листата на двоично дърво:
treeCountLeaves :: (Num b) => Tree a -> b
treeCountLeaves Empty = 0
treeCountLeaves (Node _ Empty Empty) = 1
treeCountLeaves (Node _ left right) = treeCountLeaves left + treeCountLeaves right

-- Сбор на всички стойности на върховете на двоично дърво:
treeSum :: Num a => Tree a -> a
treeSum Empty = 0
treeSum (Node x left right) = x + treeSum left + treeSum right

-- Списък със стойностите на всички вървове на ниво k:
nodesOnLevel :: (Integral b) => Tree a -> b -> [a]
nodesOnLevel Empty _ = []
nodesOnLevel (Node v left right) k
 | k < 0     = []
 | k == 0    = [v]
 | otherwise = nodesOnLevel left (k-1) ++ nodesOnLevel right (k-1)  


{- Задачи -}
{-
Задача 1. Дефинирайте функцията listSpecial tree, която приема двоично дърво tree
и връща списък от тези стойности на върховете на tree, които са равни на сбора на 
стойностите на децата си.

Примери:
tree11 = (Node 3 
            (Node 1 
                (Node 1 Empty Empty)
                Empty)
            (Node 2 
                (Node 4 Empty Empty)
                (Node 7 Empty Empty)))

listSpecial tree11 -> [3, 1]
-}

listSpecial :: (Eq a, Num a) => Tree a -> [a]
listSpecial Empty = []
listSpecial t@(Node _ lt rt) = (listSpecial lt) ++ (check t) ++ (listSpecial rt)
    where
        check :: (Eq a, Num a) => Tree a -> [a]
        check (Node x lt rt) = if x == (getChild lt) + (getChild rt) then [x] else []
            where
                getChild::(Eq a, Num a)=>Tree a-> a
                getChild Empty = 0;
                getChild (Node x _ _ ) = x

-- listSpecial :: (Eq a, Num a) => Tree a -> [a]
-- listSpecial Empty = []
-- listSpecial tree@(Node v lt rt) = if v == sum children then v : specialCh else specialCh 
--     where
--         children = nodesOnLevel tree 1
--         specialCh = listSpecial lt ++ listSpecial rt


{-
Задача 2. Дефинирайте функцията areMirrorImages tree1 tree2, която приема две двойчни
дървета tree1 и tree2 и връща дали те са огледални образи едно на друго.

Примери:
tree21 = (Node 4 
            (Node 3 
                (Node 1 Empty Empty)
                (Node 2 Empty Empty))
            (Node 5 
                Empty
                (Node 6 Empty Empty)))
tree22 = (Node 4 
            (Node 5 
                (Node 6 Empty Empty)
                Empty)
            (Node 3 
                (Node 2 Empty Empty)
                (Node 1 Empty Empty)))
tree23 = (Node 4 
            (Node 5 
                (Node 6 Empty Empty)
                Empty)
            (Node 3 
                (Node 1 Empty Empty)
                (Node 2 Empty Empty)))

areMirrorImages tree21 tree22 -> True
areMirrorImages tree21 tree23 -> False    
-}
areMirrorImages :: (Eq a) => Tree a -> Tree a -> Bool
areMirrorImages tree1 tree2 = let mirror = mirrorBst tree1
                                  eq = isEqual mirror tree2
                              in eq 
    where
        mirrorBst :: Tree a -> Tree a
        mirrorBst Empty = Empty
        mirrorBst (Node x lt rt) = (Node x (mirrorBst rt) (mirrorBst lt))

        isEqual :: (Eq a) => Tree a -> Tree a -> Bool
        isEqual Empty Empty = True
        isEqual (Node x xlt xrt) (Node y ylt yrt) = x==y && isEqual xlt ylt && isEqual xrt yrt
{-
Пордредени (сортирани) двоични дървета: 
- Всеки вътрешен възел складира ключ (и по избор и стойност свъзрана с ключа).
- Ключът във всеки възел трябва да е по-голям от всички ключове пазени влявото поддърво 
и по-малък от всички ключове пазени в дясното поддърво.
-}

{-
Задача 3. Дефинирайте функцията isBST tree, която приема двоично дърво tree и връща
дали е подредено.

Примери:
tree31 = (Node 4 
            (Node 2 
                (Node 1 Empty Empty)
                (Node 3 Empty Empty))
            (Node 5 
                Empty
                (Node 6 Empty Empty)))
tree32 = (Node 4 
            (Node 2 
                (Node 1 Empty Empty)
                (Node 3 Empty Empty))
            (Node 5 
                (Node 6 Empty Empty)
                Empty))

isBST tree31 -> True
isBST tree32 -> False
-}
-- isBST :: (Ord a, Num a) => Tree a -> Bool
-- isBST Empty = True
-- isBST (Node v lt rt) = if sum (children lt) < v && v < sum (children rt) then isBST lt && isBST rt else False
-- isBST (Node v _ _) = True
--     where
--         children::Tree a -> [a]
--         children tree = nodesOnLevel tree 0

isBST :: (Ord a, Num a) => Tree a -> Bool
isBST Empty = True
isBST (Node v lt rt) =  isBSTLeft lt v && isBSTRight rt v
    where
        isBSTLeft Empty _ =True
        isBSTLeft (Node v lt rt) max = v < max && isBSTLeft lt v && isBSTMiddle rt v max
        isBSTRight Empty _ =True
        isBSTRight (Node v lt rt) min = v > min && isBSTMiddle lt min v && isBSTRight rt v 
        isBSTMiddle Empty _ _ =True
        isBSTMiddle (Node v lt rt) min max = v > min && v < max && isBSTMiddle lt min v && isBSTMiddle rt v max 

        -- children::Tree a -> [a]
        -- children tree = nodesOnLevel tree 0
{-
Задача 4. Дефнирайте функцията bstToList tree, която приема подредено 
двоично дърво tree и връща списък с ключовете на дървото подредени във
възходящ ред.

Примери: 
tree   = (Node 4 
            (Node 2 
                (Node 1 Empty Empty)
                (Node 3 Empty Empty))
            (Node 5 
                Empty
                (Node 6 Empty Empty)))

bstToList tree -> [1, 2, 3, 4, 5, 6]
-}
bstToList :: (Ord a) => Tree a -> [a]
bstToList Empty = []
bstToList (Node v lt rt) = bstToList lt ++ [v] ++bstToList rt


{-
Задача 5. Дефинирайте функцията bstSearch tree value, която приема подредено
двоично дърво tree и стойност value и връща дали стойността се среща в дървото.

Примери:
tree   = (Node 4 
            (Node 2 
                (Node 1 Empty Empty)
                (Node 3 Empty Empty))
            (Node 5 
                Empty
                (Node 6 Empty Empty)))

bstSearch tree 1 -> True
bstSearch tree 4 -> True
bstSearch tree 7 -> False
-}
bstSearch :: (Ord a) => Tree a -> a -> Bool
bstSearch  Empty _ = False
bstSearch (Node v lt rt) value
 | value == v = True
 | value < v = bstSearch lt value
 | otherwise = bstSearch rt value


{-
Задача 6. Дефинирайте функцията bstInsert tree value, която приема подредено
двоично дърво tree и стойност value и добавя value в дървото (като го оставя
наредено).

Примери:
tree   = (Node 4 
            (Node 2 
                (Node 1 Empty Empty)
                (Node 3 Empty Empty))
            (Node 5 
                Empty
                (Node 6 Empty Empty)))

bstInsert tree 7 -> (Node 4 
                        (Node 2 
                            (Node 1 Empty Empty) 
                            (Node 3 Empty Empty)) 
                        (Node 5 
                            Empty 
                            (Node 6 
                                Empty 
                                (Node 7 Empty Empty))))
-}
bstInsert :: (Ord a) => Tree a -> a -> Tree a
bstInsert Empty value = (Node value Empty Empty)
bstInsert tree@(Node v lt rt) value
 | value < v = (Node v (bstInsert lt value) rt)
 | value > v = (Node v lt (bstInsert rt value))
 | otherwise = tree


{-
Задача 7. Дефинирайте функцията bstRemove tree value, която приема подредено
двоично дърво tree и стойност value и премахва value от дървото (като го оставя
наредено).

Примери:
tree   = (Node 4 
            (Node 2 
                (Node 1 Empty Empty)
                (Node 3 Empty Empty))
            (Node 5 
                Empty
                (Node 6 Empty Empty)))

bstRemove tree 4 -> (Node 2 
                        (Node 1 Empty Empty) 
                        (Node 3 
                            Empty 
                            (Node 5 
                                Empty 
                                (Node 6 Empty Empty))))
-}
--na Toni
bstRemove :: (Ord a) => Tree a -> a -> Tree a
bstRemove (Node v Empty Empty) _ = Empty
bstRemove (Node v left right) value
    | v == value = removeSubtree (Node v left right)
    | v > value = (Node v (bstRemove left value) right)
    | v < value = (Node v left (bstRemove right value))
        where removeSubtree (Node _ Empty right) = right
              removeSubtree (Node _ left Empty) = left
              removeSubtree (Node _ left right) = (Node leftest left (bstRemove right leftest))
                    where leftestElement (Node v Empty _) = v
                          leftestElement (Node _ left _) = leftestElement left
                          leftest = leftestElement right


-- main функция с примерни извиквания на функциите от задачите.

main = let
    tree11 = (Node 3 
                (Node 1 
                    (Node 1 Empty Empty)
                    Empty)
                (Node 2 
                    (Node 4 Empty Empty)
                    (Node 7 Empty Empty)))
    tree21 = (Node 4 
                (Node 3 
                    (Node 1 Empty Empty)
                    (Node 2 Empty Empty))
                (Node 5 
                    Empty
                    (Node 6 Empty Empty)))
    tree22 = (Node 4 
                (Node 5 
                    (Node 6 Empty Empty)
                    Empty)
                (Node 3 
                    (Node 2 Empty Empty)
                    (Node 1 Empty Empty)))
    tree23 = (Node 4 
                (Node 5 
                    (Node 6 Empty Empty)
                    Empty)
                (Node 3 
                    (Node 1 Empty Empty)
                    (Node 2 Empty Empty)))
    tree31 = (Node 4 
                (Node 2 
                    (Node 1 Empty Empty)
                    (Node 3 Empty Empty))
                (Node 5 
                    Empty
                    (Node 6 Empty Empty)))
    tree32 = (Node 4 
                (Node 2 
                    (Node 1 Empty Empty)
                    (Node 3 Empty Empty))
                (Node 5 
                    (Node 6 Empty Empty)
                    Empty))
    -- Зад. 4-7.
    tree   = (Node 4 
                (Node 2 
                    (Node 1 Empty Empty)
                    (Node 3 Empty Empty))
                (Node 5 
                    Empty
                    (Node 6 Empty Empty)))
    in do
        -- Задача 1.
        print $ listSpecial tree11

        -- Задача 2.
        print $ areMirrorImages tree21 tree22
        print $ areMirrorImages tree21 tree23

        -- Задача 3.
        print $ isBST tree31
        print $ isBST tree32

        -- Задача 4.
        print $ bstToList tree

        -- Задача 5.
        print $ bstSearch tree 1
        print $ bstSearch tree 4
        print $ bstSearch tree 7
        
        -- Задача 6.
        print $ bstInsert tree 7

        -- Задача 7.
        print $ bstRemove tree 4        