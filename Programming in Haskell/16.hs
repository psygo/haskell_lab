data Expr = Val Int | Add Expr Expr

eval :: Expr -> Int
eval (Val n) = n
eval (Add x y) = eval x + eval y

type Stack = [Int]

type Code = [Op]

data Op = PUSH Int | ADD
          deriving Show

exec :: Code -> Stack -> Stack
exec []           s           = s
exec (PUSH n : c) s           = exec c (n : s)
exec (ADD : c)    (m : n : s) = exec c (n+m : s)

-- Inefficient version:
comp :: Expr -> Code
comp (Val n)   = [PUSH n]
comp (Add x y) = comp x ++ comp y ++ [ADD]
