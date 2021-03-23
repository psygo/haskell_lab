-- 3
numberFunc = a `div` length xs
                where
                  a = 10
                  xs = [1,2,3,4,5]

-- 4
last' xs = xs !! (length xs - 1)

last'' xs = head (reverse xs)

last''' :: [a] -> a
last''' (x:xs) = if length xs == 0
                   then x
                   else last''' xs

-- 5
init' xs = take (length xs - 1) xs

init'' xs = reverse (drop 1 (reverse xs))
