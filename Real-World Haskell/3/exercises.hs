import Data.List

data BookInfo = Book Int String [String]
  deriving (Show)

myInfo :: BookInfo
myInfo = Book 9780135072455 "Algebra of Programming" ["Richard Bird", "Oege de Moor"]

-- Type Synonyms
type CustomerID = Int

type ReviewBody = String

data BetterReview = BetterReview BookInfo CustomerID ReviewBody

type BookRecord = BookInfo

----------------------------------------------

-- Algebraic Data Types

type CardHolder = String

type CardNumber = String

type Address = [String]

data BillingInfo
  = CreditCard CardNumber CardHolder Address
  | CashOnDelivery
  | Invoice CustomerID
  deriving (Show)

----------------------------------------------

-- Enumeration

data Roygbiv
  = Red
  | Orange
  | Yellow
  | Green
  | Blue
  | Indigo
  | Violet
  deriving (Eq, Show)

--------------------------------------------------

-- Pattern Matching

myNot True = False
myNot False = True

sumList (x : xs) = x + sumList xs
sumList [] = 0

-- Extracting values from named constructors
bookID (Book id title authors) = id

bookTitle (Book id title authors) = title

bookAuthors (Book id title authors) = authors

-- Defining accessors in the constructor
data Customer = Customer
  { customerID :: CustomerID,
    customerName :: String,
    customerAddress :: [String]
  }
  deriving (Show)

-- try :t customerID on GHCi. You'll get customerID :: Customer -> CustomerID

-- You can also use a more "JSON-like" syntax for initializing objects:
customer2 =
  Customer
    { customerID = 271828,
      customerAddress =
        [ "1048576 Disk Drive",
          "Milpitas, CA 95134",
          "USA"
        ],
      customerName = "Jane Q. Citizen"
    }

-- Parameterized Types
-- data Maybe a
--   = Just a
--   | Nothing

-----------------------------------------------

-- Recursive Types

data List a
  = Cons a (List a)
  | Nil
  deriving (Show)

example = Cons 1 (Cons 0 Nil)

-- Proving List is isomorphic to []
fromList (x : xs) = Cons x (fromList xs)
fromList [] = Nil

toList :: List a -> [a]
toList (Cons a b) = a : toList b
toList Nil = []

-- Recursive Binary Tree
data Tree a
  = Node a (Tree a) (Tree a)
  | Empty
  deriving (Show)

simpleTree =
  Node
    "parent"
    (Node "left child" Empty Empty)
    (Node "right child" Empty Empty)

data TreeWithMaybe a = NodeM a (Maybe (TreeWithMaybe a)) (Maybe (TreeWithMaybe a))
  deriving (Show)

simpleTreeWithMaybe =
  NodeM
    "parent"
    Nothing
    Nothing

simpleTreeWithMaybe2 =
  NodeM
    "parent"
    (Just (NodeM "first child" Nothing Nothing))
    Nothing

-------------------------------------------

-- Reporting Errors

mySecond xs =
  if null (tail xs)
    then error "list too short"
    else head (tail xs)

-- More controlled approach:
safeSecond [] = Nothing
safeSecond xs =
  if null (tail xs)
    then Nothing
    else Just (head (tail xs))

tidySecond (_ : x : _) = Just x
tidySecond _ = Nothing

-----------------------------------------------

-- Introducing local variables

lend amount balance =
  let reserve = 100
      newBalance = balance - amount
   in if balance < reserve
        then Nothing
        else Just newBalance

lend2 amount balance =
  if amount < reserve * 0.5
    then Just newBalance
    else Nothing
  where
    reserve = 100
    newBalance = balance - amount

--------------------------------------------

-- Explicit Structuring

foo =
  let a = 1
      b = 2
      c = 3
   in a + b + c

-----------------------------------------------

-- The Case Expression

fromMaybe defval wrapped =
  case wrapped of
    Nothing -> defval
    Just value -> value

------------------------------------------------

-- Guards

nodesAreSame (Node a _ _) (Node b _ _)
  | a == b = Just a
nodesAreSame _ _ = Nothing

------------------------------------------------

lend3 amount balance
  | amount <= 0 = Nothing
  | amount > reserve * 0.5 = Nothing
  | otherwise = Just newBalance
  where
    reserve = 100
    newBalance = balance - amount

-- Nicer version of myDrop:
niceDrop n xs | n <= 0 = xs
niceDrop _ [] = []
niceDrop n (_ : xs) = niceDrop (n - 1) xs

-------------------------------------------------

-- Excercises

-- 1 & 2
myLength :: [a] -> Int
myLength (x : xs) = 1 + myLength xs
myLength [] = 0

-- Or:
-- myLength xs = foldr (\ x -> (+) 1) 0 xs

-- 3
myMean :: [Double] -> Double
myMean [] = 0
myMean list = sum list / fromIntegral (length list)
  where
    sum (x : xs) = x + sum xs
    sum [] = 0
    length (x : xs) = 1 + length xs
    length [] = 0

-- 4
palindrome :: [a] -> [a]
palindrome [] = []
palindrome list = list ++ reversed list
  where
    reversed :: [a] -> [a]
    reversed [] = []
    reversed (x : xs) = reversed xs ++ [x]

-- palindrome list = list ++ reverse list

-- 5
checkIfPalindrome :: [Int] -> Bool
checkIfPalindrome [] = True
checkIfPalindrome list = left == reversedRight
  where
    left = take (length list `div` 2) list
    reversedRight = reverse (drop (length list `div` 2) list)
      where
        length :: [Int] -> Int
        length (x : xs) = 1 + length xs
        length [] = 0

-- 6
sortByLength :: [[a]] -> [[a]]
sortByLength [] = []
sortByLength globalList = sortBy compareLists globalList
  where
    compareLists :: [a] -> [a] -> Ordering
    compareLists listLeft listRight = compare (length listLeft) (length listRight)

-- 7 & 8
intersp :: a -> [[a]] -> [a]
intersp separator (x : xs)
  | null xs = x
  | otherwise = x ++ [separator] ++ intersp separator xs

-- 9
height :: Tree a -> Int
height Empty = 0
height (Node tree leftChild rightChild) = 1 + greater (height leftChild) (height rightChild)
  where
    greater :: Int -> Int -> Int
    greater x y =
      if x >= y
        then x
        else y

-- 10
data Direction = Left | Right | Straight

-- 11
-- TODO

-- 12
-- TODO