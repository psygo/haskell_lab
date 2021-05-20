module Lib
    ( grid
    , languages
    , formatGrid
    , outputGrid
    , findWord
    , findWords
    ) where

import Data.List (isInfixOf, transpose)
import Data.Maybe (catMaybes)

type Grid = [String]

findWord :: Grid -> String -> Maybe String
findWord grid word =
  let lines = getLines grid
      found = any (findWordInLine word) lines
  in if found then Just word else Nothing

getLines :: Grid -> [String]
getLines grid =
  let horizontal = grid
      vertical   = transpose grid
      diagonal1  = diagonalize grid
      diagonal2  = diagonalize $ map reverse grid
      lines      = horizontal ++ vertical ++ diagonal1 ++ diagonal2
  in lines ++ map reverse lines

diagonalize :: Grid -> Grid
diagonalize = transpose . skew

-- Dealing with diagonals:
skew :: Grid -> Grid
skew [] = []
skew (l:ls) = l : skew (map indent ls)
  where indent line = '_' : line
  
-- findWords :: Grid -> [String] -> [Bool]
findWords grid words =
  let foundWords = map (findWord grid) words
  in  catMaybes foundWords

findWordInLine :: String -> String -> Bool
findWordInLine = isInfixOf

formatGrid :: Grid -> String
formatGrid = unlines

outputGrid :: Grid -> IO ()
outputGrid grid = putStrLn (formatGrid grid)
