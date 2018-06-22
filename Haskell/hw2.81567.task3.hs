
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Read, Show)

isBalanced :: Tree a -> Int -> Bool
isBalanced Empty _ = True
isBalanced (Node _ lt rt) k = (abs ((btHeight lt) - (btHeight rt))) <= k  &&  (isBalanced lt k) && (isBalanced rt k)

btHeight:: Tree a-> Int 
btHeight Empty = 0
btHeight (Node _ Empty Empty) = 0
btHeight (Node _ lt rt) = 1 + (max (btHeight lt) (btHeight rt))
