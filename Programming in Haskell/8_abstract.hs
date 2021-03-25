--------------------------------------------------------------------------------
-- An abstract machine to specify the order of evaluation,
-- instead of leaving it to Haskell itself.

data Expr = Val Int | Add Expr Expr

value :: Expr -> Int
value (Val n) = n
value (Add x y) = value x + value y
--------------------------------------------------------------------------------
-- Control Stacks

type Cont = [Op]

data Op = EVAL Expr | ADD Int

eval :: Expr -> Cont -> Int
eval (Val n) c = exec c n
eval (Add x y) c = eval x (EVAL y : c)

exec :: Cont -> Int -> Int
exec []           n = n
exec (EVAL y : c) n = eval y (ADD n : c)
exec (ADD  n : c) m = exec c (n+m)
--------------------------------------------------------------------------------
