add x y = x + y

-- replicates the `drop` function's behavior:
myDrop n xs = if n <= 0 || null xs
              then xs
              else myDrop (n - 1) (tail xs)

-- My recursive version of a sum
mySum x y = if y == 0
            then x
            else mySum (x + 1) (y - 1)

myMult x y = if y == 1
             then x
             else myMult (x + x) (y - 1)