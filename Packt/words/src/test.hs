import Control.Monad (guard)

data Cell = Cell (Integer,Integer) Char deriving (Eq,Ord,Show)

og :: Show a -> [a] -> IO ()
og = putStrLn . unlines . map show

-- List is also a monad
mapped = do
  i <- [0..]
  return (i * 2)

filter = do
  i <- [0..]
  guard (div2 i)
  return i

coords2 = do
  row <- [0..7]
  return $ do
    col <- [0..7]
    return (row,col)

-- `zip` is `zipWith` with the `(,)` operator.
zipOverGrid :: [[a]] -> [[b]] -> [[(a,b)]]
zipOverGrid = zipWith (,)

zipOverGridWith = zipWith . zipWith
  
-- zipOverGrid = zipWith zip
