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