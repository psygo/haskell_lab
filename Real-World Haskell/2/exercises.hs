add x y = x + y

-- replicates the `drop` function's behavior:
myDrop n xs = if n <= 0 || null xs
              then xs
              else myDrop (n - 1) (tail xs)

-- Exercise 2 on page 39
-- I don't think this is the way the book expected me to do this...
lastButOne list = list !! (length list - 2)
