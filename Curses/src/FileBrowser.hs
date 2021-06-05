{-# LANGUAGE OverloadedStrings #-}

module FileBrowser where

import System.Directory
import System.Exit

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Cursor.Simple.List.NonEmpty

import Brick.AttrMap
import Brick.Main
import Brick.Types
import Brick.Util
import Brick.Widgets.Core
import Graphics.Vty.Attributes
import Graphics.Vty.Input.Events

main :: IO ()
main = do
  initialState <- buildInitialState
  endState <- defaultMain tuiApp initialState
  print endState

data TuiState = TuiState
  { tuiStatePaths :: NonEmptyCursor FilePath
  } deriving (Show, Eq)

type ResourceName = String

tuiApp :: App TuiState e ResourceName
tuiApp =
  App
    { appDraw = drawTui
    , appChooseCursor = showFirstCursor
    , appHandleEvent = handleTuiEvent
    , appStartEvent = pure
    , appAttrMap = const $ attrMap mempty [("selected", fg red)]
    }

buildInitialState :: IO TuiState
buildInitialState = do
  here <- getCurrentDirectory
  contents <- getDirectoryContents here
  case NE.nonEmpty contents of
    Nothing -> die "There are no contents."
    Just ne -> pure TuiState {tuiStatePaths = makeNonEmptyCursor ne}
  -- NE.NonEmpty :: [a] -> Maybe (NonEmpty a)
  -- pure TuiState {tuiStatePaths = contents}

-- vBox :: [Widget n] -> Widget n
drawTui :: TuiState -> [Widget ResourceName]
drawTui ts =
  let nec = tuiStatePaths ts
   in [ vBox $
        concat
            [ map (drawPath False) $ reverse $ nonEmptyCursorPrev nec
            , [drawPath True $ nonEmptyCursorCurrent nec]
            , map (drawPath False) $ nonEmptyCursorNext nec
            ]
      ]

drawPath :: Bool -> FilePath -> Widget n
drawPath b =
  (if b
       then withAttr "selected"
       else id) .
  str

handleTuiEvent :: TuiState -> BrickEvent n e -> EventM n (Next TuiState)
handleTuiEvent s e =
  case e of
    VtyEvent vtye -> 
      case vtye of
        EvKey (KChar 'q') [] -> halt s
        EvKey KDown [] -> do
          let nec = tuiStatePaths s
          case nonEmptyCursorSelectNext nec of
            Nothing -> continue s
            Just nec' -> continue $ s {tuiStatePaths = nec'}
        EvKey KUp [] -> do
          let nec = tuiStatePaths s
          case nonEmptyCursorSelectPrev nec of
            Nothing -> continue s
            Just nec' -> continue $ s {tuiStatePaths = nec'}
        _ -> continue s
    _ -> continue s
