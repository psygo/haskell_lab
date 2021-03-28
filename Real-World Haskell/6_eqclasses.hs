-- Typeclasses look like mixins as interfaces
-- or abstract mixins?
class BasicEq a where
  isEqual :: a -> a -> Bool

-- isEqual :: (BasicEq a) => a -> a -> Bool

instance BasicEq Bool where
  isEqual True True = True
  isEqual False False = True
  isEqual _ _ = False

--------------------------------------------------------------------------------

class BasicEq2 a where
  isEqual2 :: a -> a -> Bool
  isNotEqual2 :: a -> a -> Bool

--------------------------------------------------------------------------------

-- People implementing this class must provide an implementation of at least
-- one function. They can implement both if they wish, but they will not be
-- required to
class BasicEq3 a where
  isEqual3 :: a -> a -> Bool
  isEqual3 x y = not (isNotEqual3 x y)

  isNotEqual3 :: a -> a -> Bool
  isNotEqual3 x y = not (isEqual3 x y)

--------------------------------------------------------------------------------

main = do
  putStrLn "Please enter a Double:"
  inpStr <- getLine
  let inpDouble = read inpStr :: Double
  putStrLn ("Twice " ++ show inpDouble ++ " is " ++ show (inpDouble * 2))

--------------------------------------------------------------------------------

-- Using overlapping instances to help the JSON code
newtype JAry a = JAry
  { fromJAry :: [a]
  }
  deriving (Eq, Ord, Show)