--------------------------------------------------------------------------------
-- Data

data Prop = Const Bool
          | Var   Char
          | Not   Prop
          | And   Prop Prop
          | Imply Prop Prop

-- A ^ -A
p1 = And (Var ’A’) (Not (Var ’A’))

-- (A ^ B) => A
p2 = Imply (And (Var ’A’) (Var ’B’)) (Var ’A’)

-- A => (A ^ B)
p3 = Imply (Var ’A’) (And (Var ’A’) (Var ’B’))

-- (A ^ (A => B)) => B
p4 = Imply (And (Var ’A’) (Imply (Var ’A’) (Var ’B’))) (Var ’B’)

type Subst = Assoc Char Bool
--------------------------------------------------------------------------------
eval :: Subst -> Prop -> Bool
eval _ (Const b)   = b
eval s (Var x)     = find x s
eval s (Not p )    = not (eval s p)
eval s (Not p)     = not (eval s p)
eval s (And p q)   = eval s p && eval s q
eval s (Imply p q) = eval s p <= eval s q

-- This does *not* deal with duplicates.
vars :: Prop -> [Char]
vars (Const _)   = []
vars (Var x)     = [x]
vars (Not p)     = vars p
vars (And p q)   = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q

-- All combinations are analogous to binary numbers
bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False:) bss ++ map (True:) bss
          where bss = bools (n-1)

rmdups :: Eq a => [a] -> [a]
rmdups []     = []
rmdups (x:xs) = x : filter (/= x) (rmdups xs)

susbts :: Prop -> [Subst]
susbts p = map (zip vs) (bools (length vs))
           where vs = rmdups (vars p)

isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substs p]
--------------------------------------------------------------------------------
