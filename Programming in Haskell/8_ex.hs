-- 1
mult m Zero = Zero
mult m (Succ n) = add m (mult m n)

-- 2
occurs x (Leaf y) = x == y
occurs (Node l y r) = case compare x y of
                        LT -> occurs x l
                        EQ -> True
                        GT -> occurs x r
-- This version is mor efficient because it only requires one
-- comparison between x and y for each note, whereas the previous
-- may require two.

-- 3
leaves (Leaf _) = 1
leaves (Node l r) = leaves l + leaves r

balanced (Leaf _) = True
balanced (Node l r) = abs (leaves l - leaves r) <= 1
                      && balanced l && balanced r

-- 4
halve xs = splitAt (length xs `div` 2) xs

balance [x] = Leaf x
balance xs  = Node (balance ys) balance(zs)
              where (ys,zs) = halve xs
