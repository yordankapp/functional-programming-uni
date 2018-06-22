
splits:: [a] -> [([a],[a])]
splits xs = helper xs 0
    where
        helper :: [a] -> Int -> [([a],[a])]
        helper xs i
         | i == length xs = (splitAt i xs) :[]
         | otherwise = (splitAt i xs) : helper xs (i+1)

increasing:: (a->a) -> a -> a -> Bool
increasing f a b = helper (map f [a..b])
    where 
        helper :: [a] -> Bool
        helper xs 
         | null (tail xs) = True
         | (head xs) <= (head (tail xs)) = helper  (tail xs)
         | otherwise = False

main = do
    print $ splits [1,2,3]
