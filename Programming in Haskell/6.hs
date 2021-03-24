--------------------------------------------------------------------------------
insert :: Ord a => a -> [a] -> [a]
insert x []                 = [x]
insert x (y:ys) | x <= y    = x:y:ys
                | otherwise = y : insert x ys

isort :: Ord a => [a] -> [a]
isort []     = []
isort (x:xs) = insert x (isort xs)
--------------------------------------------------------------------------------
-- Multiple Arguments

zip' :: [a] -> [b] -> [(a,b)]
zip' []     _      = []
zip' _      []     = []
zip' (x:xs) (y:ys) = (x,y) : zip xs ys
--------------------------------------------------------------------------------
-- Mutual Recursion

even' :: Int -> Bool
even' 0 = True
even' n = odd' (n-1)

odd' :: Int -> Bool
odd' 0 = False
odd' n = even' (n-1)
--------------------------------------------------------------------------------
-- Exercises

-- 4
euclid :: Int -> Int -> Int
euclid a b | a == b    = a
           | otherwise = if (b > a) then euclid (b-a) a else euclid (a-b) b

-- 7
merge :: Ord a => [a] -> [a] -> [a]
merge left  [] = left
merge [] right = right
merge (x:xs) (y:ys) | x < y     = x:(merge xs (y:ys))
                    | otherwise = y:(merge (x:xs) ys)

-- 8
halve :: [a] -> ([a],[a])
halve xs = ((take n xs),(drop n xs))
  where n = length xs `div` 2

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort first) (msort second)
  where (first, second) = halve xs
