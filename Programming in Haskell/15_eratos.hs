primes :: [Int]
primes = sieve [2..]

sieve :: [Int] -> [Int]
sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

primesUpToN :: Int -> [Int]
primesUpToN n = takeWhile (< n) primes
