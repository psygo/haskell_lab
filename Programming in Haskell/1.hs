--------------------------------------------------------------------------------
-- Returning zero as the sum of the empty list is appropriate because zero is
-- the identity for addition
sum' [] = 0
sum' (n:ns) = n + sum' ns
--------------------------------------------------------------------------------
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
               where
                 smaller = [a | a <- xs, a <= x]
                 larger  = [b | b <- xs, b >  x]
--------------------------------------------------------------------------------
seqn :: Monad m => [m a] -> m [a]
seqn []         = return []
seqn (act:acts) = do x  <- act
                     xs <- seqn acts
                     return (x:xs)
--------------------------------------------------------------------------------
-- Exercises

-- 3
product' [] = 1
product' (x:xs) = x * product' xs

-- 4
qsortReverse :: Ord a => [a] -> [a]
qsortReverse []     = []
qsortReverse (x:xs) = qsortReverse larger ++ [x] ++ qsortReverse smaller
               where
                 smaller = [a | a <- xs, a <= x]
                 larger  = [b | b <- xs, b >  x]

-- 5
-- Removing an equal evaluation filters out repeated values:
-- [2,2,3,1,1] -> [1,2,3]
qsortLess :: Ord a => [a] -> [a]
qsortLess []     = []
qsortLess (x:xs) = qsortLess smaller ++ [x] ++ qsortLess larger
               where
                 smaller = [a | a <- xs, a <  x]
                 larger  = [b | b <- xs, b >  x]
--------------------------------------------------------------------------------
