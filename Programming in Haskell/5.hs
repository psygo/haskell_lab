--------------------------------------------------------------------------------
exampleSet = [x^2 | x <- [1..5]]

-- Changing the order of the pairing doesn't make a difference.
examplePairing = [(x,y) | y <- [4,5], x <- [1,2,3]]

parameterizedGenerator = [(x,y) | x <- [1..3], y <- [x..3]]

-- Concatenating a list of lists
concat :: [[a]] -> [a]
concat xss = [x | xs <- xss, x <- xs]


factors :: Int -> [Int]
factors n = [x | x <- [1..n], n ‘mod‘ x == 0]

prime :: Int -> Bool
prime n = factors n == [1,n]
-- Note that deciding that a number such as 15 is not prime does not require the
-- function prime to produce all of its factors, because under lazy evaluation the
-- result False is returned as soon as any factor other than one or the number itself
-- is produced, which for this example is given by the factor 3.

primes :: Int -> [Int]
primes n = [x | x <- [2..n], prime x]

find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k',v) <- t, k == k']


pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)

sorted :: Ord a => [a] -> Bool
sorted xs = and [x <= y | (x,y) <- pairs xs]

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x',i) <- zip xs [0..], x == x']


count :: Char -> String -> Int
count x xs = length [x' | x' <- xs, x == x']
--------------------------------------------------------------------------------
