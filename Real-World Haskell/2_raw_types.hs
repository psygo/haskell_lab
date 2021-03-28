-- All you do with a boolean is make a choice
true x y = x
false x y = y

ifte bool t e = bool t e

two f x = f (f x)
one f x = f x
zero f x = x

-- two (+1) 0 = 2

add m n f x = m f (n f x)
mult m n f x = m (n f) x

-- add one (mult two two) (+1) 0 = 5

iszero n = n (\_ -> false) true
-- decr n = n (\m f x -> f (m incr zero))
--   zero
--   (\x -> x)
--   zero
-- fact n = ifte (iszero n) one (mult n (fact (decr n)))

-- My recursive version of a sum
-- It uses sums and subtractions internally though...
mySum x y = if y == 0
            then x
            else mySum (x + 1) (y - 1)

-- My recursive version of multiplication
myMult x y = if y == 1
             then x
             else x + myMult x (y - 1)
