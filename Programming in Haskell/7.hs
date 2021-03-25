--------------------------------------------------------------------------------
{-
map :: (a -> b) -> [a] -> [b]
map f xs = [f x | x <- xs]
-}

map :: (a -> b) -> [a] -> [b]
map f []     = []
map f (x:xs) = f x : map f xs

{-
filter :: (a -> Bool) -> [a] -> [a]
filter p xs = [x | x <- xs, px]
-}

filter :: (a -> Bool) -> [a] -> [a]
filter p []                 = []
filter p (x:xs) | p x       = x : filter p xs
                | otherwise = filter p xs
--------------------------------------------------------------------------------
