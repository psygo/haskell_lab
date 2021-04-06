binToBool :: [Int] -> [Bool]
binToBool []     = []
binToBool (x:xs) | x == 0    = False:(binToBool xs)
                 | otherwise = True :(binToBool xs)

boolToBin :: [Bool] -> [Int]
boolToBin []     = []
boolToBin (x:xs) | x == False = 0:(boolToBin xs)
                 | otherwise  = 1:(boolToBin xs)

xorSingle :: Bool -> Bool -> Bool
xorSingle False True  = True
xorSingle True  False = True
xorSingle _     _     = False

xorList :: [Bool] -> [Bool] -> [Bool]
xorList []     _     = []
xorList _      []    = []
xorList (x:xs) (y:ys) = (xorSingle x y):(xorList xs ys)

completeXor :: [Int] -> [Int] -> [Int]
completeXor xs ys = boolToBin (xorList (binToBool xs) (binToBool ys))
