-- 1
sumSquares = sum [x^2 | x <- [1..100]]

-- 2
grid :: Int -> Int -> [(Int,Int)]
grid maxX maxY = [(x,y) | x <- [0..maxX], y <- [0..maxY]]

-- 3
square :: Int -> [(Int,Int)]
square n = [(x, y) | (x, y) <- grid n n, x /= y]

-- 4
replicate' :: Int -> a -> [a]
replicate' n el = [el | _ <- [1..n]]

-- 5
pyths :: Int -> [(Int,Int,Int)]
pyths limit = [(x,y,z) | x <- maxList, y <- maxList, z <- maxList, x^2 + y^2 == z^2]
               where maxList = [1..limit]

-- 6
factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects limit = [perfect | perfect <- [1..limit], perfect == sum (factorsExceptItself perfect)]
                  where factorsExceptItself n = filter (\x -> x /= n) (factors n)

-- 9
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x*y | (x,y) <- zip xs ys]
