--------------------------------------------------------------------------------
import CustomTests
--------------------------------------------------------------------------------
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
--------------------------------------------------------------------------------
-- Tests

xorTests :: [Test]
xorTests  = [
              Test "0 0 -> 0" ((completeXor [0] [0]) == [0])
            , Test "0 1 -> 1" ((completeXor [0] [1]) == [1])
            , Test "1 0 -> 1" ((completeXor [1] [0]) == [1])
            , Test "1 1 -> 0" ((completeXor [1] [1]) == [0])
            , Test "0011 0101 -> 0110" 
                   ((completeXor [0,0,1,1] [0,1,0,1]) == [0,1,1,0])
            ]

runTests :: IO ()
runTests =  testsResults xorTests
--------------------------------------------------------------------------------
