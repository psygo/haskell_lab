module Models.Models where

import Data.Char
import Data.List
import Data.Maybe

import Utils.Utils

data IntersectionState    = Empty | Black | White deriving (Show, Eq)
type IntersectionStateRow = [IntersectionState]
type IntersectionMatrix   = [IntersectionStateRow]
type TernaryRow           = [Int]
type TernaryMatrix        = [TernaryRow]

ternaryToIntersectionMatrix :: TernaryMatrix -> IntersectionMatrix
ternaryToIntersectionMatrix =  map ternaryRowToIntersectionRow
 where
   ternaryRowToIntersectionRow :: TernaryRow -> IntersectionStateRow
   ternaryRowToIntersectionRow []     = []
   ternaryRowToIntersectionRow (t:ts) = case t of
     0 -> Empty : ternaryRowToIntersectionRow ts
     1 -> Black : ternaryRowToIntersectionRow ts
     2 -> White : ternaryRowToIntersectionRow ts

modifyRow :: Int -> Int -> TernaryRow -> TernaryRow
modifyRow player pos row = take (pos-1) row ++ [player] ++ drop pos row

parseCoords :: (Char,Char) -> Int -> (Int,Int)
parseCoords (x,y) boardSize =
  (
    boardSize - digitToInt y + 1 :: Int
  , fromMaybe 1 index + 1        :: Int
  )
    where
      index :: Maybe Int
      index =  elemIndex x (filter (/=' ') (alphabetCoordinates boardSize))

modifyBoard :: String -> String -> TernaryMatrix -> TernaryMatrix
modifyBoard player coords board =
     take (rowCoord-1) board
  ++ [modifyRow playerAsInt colCoord (board !! (rowCoord-1))]
  ++ drop rowCoord board
    where
      rowCoord :: Int
      rowCoord =  fst coordsAsInt

      colCoord :: Int
      colCoord =  snd coordsAsInt

      coordsAsInt :: (Int,Int)
      coordsAsInt =  parseCoords (head coords, last coords) (length board)

      playerAsInt :: Int
      playerAsInt |  player == "Black" = 1
                  |  otherwise         = 2
