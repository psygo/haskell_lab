--------------------------------------------------------------------------------
-- Conditional Expressions

signum' :: Int -> Int
signum' n = if n < 0 then -1 else
               if n == 0 then 0 else 1

signum'' n | n < 0     = -1
           | n == 0    = 0
           | otherwise = 1

{-
True && True = True
_    && _    = False
-}
--------------------------------------------------------------------------------
-- Exercises

-- 1
halve xs = (take halfLength xs, drop halfLength xs)
   where halfLength = length xs `div` 2

-- 2
third' xs = head (drop 2 xs)

third'' xs = xs !! 2

third''' (x1:x2:x3:xs) = x3

-- 3
safetail xs = if null xs then [] else tail xs

safetail' xs | null xs   = []
             | otherwise = tail xs

safetail'' [] = []
safetail'' xs = tail xs

-- 4
True  || True  = True
True  || False = True
False || True  = True
False || False = False

-- 5
-- left && right = if left == True then (if right == True then True else False) else False

-- 6
True && b = if b == True then True else False
False && _ = False

-- 7
mult :: Int -> Int -> Int -> Int
mult = \x -> (\y -> (\z -> x * y * z))

-- 8
luhnDouble :: Int -> Int
luhnDouble n | doubled > 9 = doubled - 9
             | otherwise   = doubled
               where doubled = 2*n

luhn :: Int -> Int -> Int -> Int -> Bool
luhn n1 n2 n3 n4 | total `mod` 10 == 0 = True
                 | otherwise           = False
  where
    luhnDouble1 = luhnDouble n3
    luhnDouble3 = luhnDouble n1
    total       = luhnDouble1 + n2 + luhnDouble3 + n4
