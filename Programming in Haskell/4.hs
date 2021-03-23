--------------------------------------------------------------------------------
-- Conditional Expressions

signum' :: Int -> Int
signum' n = if n < 0 then -1 else
               if n == 0 then 0 else 1

signum'' n | n < 0     = -1
           | n == 0    = 0
           | otherwise = 1

True && True = True
_    && _    = False
--------------------------------------------------------------------------------
