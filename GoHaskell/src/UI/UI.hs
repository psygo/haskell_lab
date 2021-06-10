module UI.UI where

import Control.Monad

import Configs.Config
import Models.Models
import Utils.Utils

alterBoardInteractively :: TernaryMatrix -> IO ()
alterBoardInteractively board = do
  putStrLn ""
  printBoardFromTernary board
  putStrLn "Please type in your next move (e.g. Black B2):"
  playerAndCoords <- getLine
  let player      =  head $ words playerAndCoords
  let coords      =  words playerAndCoords !! 1
  let newBoard    =  modifyBoard player coords board
  alterBoardInteractively newBoard

printBoardFromTernary :: TernaryMatrix -> IO ()
printBoardFromTernary = printIntersectionMatrix . ternaryToIntersectionMatrix

printIntersectionMatrix :: IntersectionMatrix -> IO ()
printIntersectionMatrix []         = return ()
printIntersectionMatrix (row:rows) = do
  putStr "\n"
  putStr $ " " ++ show (length rows + 1) ++ " "
  intersectionRowToString row
  printIntersectionMatrix rows
  putStr "\n"
  when (null rows) (putStr $ "   " ++ alphabetCoordinates (length row))
    where
      intersectionRowToString :: IntersectionStateRow -> IO ()
      intersectionRowToString []             = return ()
      intersectionRowToString (state:states) = do
        configFile <- readConfigFile
        let config = decodeConfig configFile
        case state of
          Empty -> textColorToColor $ empty_intersection_color config
          Black -> textColorToColor $ black_color              config
          White -> textColorToColor $ white_color              config
        case state of
          Empty -> putStr [empty_intersection_char config]
          Black -> putStr [black_char config]
          White -> putStr [white_char config]
        grey
        if null states
           then return ()
           else putStr [horizontal_spacing_char config]
        intersectionRowToString states
