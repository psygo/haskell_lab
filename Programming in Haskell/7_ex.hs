-- 1
filterMap' :: (a -> b) -> (a -> Bool) -> [a] -> [b]
filterMap' f p x = map f (filter p x)

-- 3
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x xs -> f x : xs) []
