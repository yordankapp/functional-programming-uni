
calcLuhnChecksum :: Int -> Int
calcLuhnChecksum n =  let digit = (reverse (listDigits n)) 
                          weight = (listWeights (length (listDigits n)) 1)
                          product = (listProduct digit weight)
                          sumDigits = 9 * sum (helper product)
                          rezult = sumDigits `mod` 10
                      in  rezult

    where 
        listDigits :: Int -> [Int]
        listDigits n 
         | n==0 = []
         | otherwise = ( n `mod` 10) : (listDigits (n `div` 10))
        
        listWeights :: Int -> Int -> [Int]
        listWeights lengthList i 
         | i==(lengthList+1) = []
         | i `mod` 2 == 0  = 2 : (listWeights lengthList (i+1))
         | otherwise = 1 : (listWeights lengthList (i+1))
        
        listProduct :: [Int] -> [Int] -> [Int]
        listProduct digit weight 
         | null digit = []
         | otherwise = ((head digit) * (head weight)) : (listProduct (tail digit) (tail weight))
        
        helper :: [Int] -> [Int]
        helper product 
         | null product = []
         | (head product) `div` 10 == 0  = (head product) : (helper (tail product))
         | otherwise = (calcNewDigit (head product) 0) : (helper (tail product))
             where
                calcNewDigit :: Int -> Int -> Int
                calcNewDigit oldDigit newDigit
                 | oldDigit == 0 = newDigit
                 | otherwise = calcNewDigit (oldDigit `div` 10) (newDigit + (oldDigit `mod` 10))
         
