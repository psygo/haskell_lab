{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Configs.Config where

import Data.Maybe
import GHC.Generics

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.UTF8 as BLU

import qualified Data.Aeson as A

import Utils.Utils

data Config = Config
  {
    black_char               :: Char
  , black_color              :: String
  , white_char               :: Char
  , white_color              :: String
  , empty_intersection_char  :: Char
  , empty_intersection_color :: String
  , horizontal_spacing_char  :: Char
  , horizontal_spacing_color :: String
  } deriving (Generic, Show)

defaultConfig :: Config
defaultConfig =  Config
  {
    black_char               = '●'
  , black_color              = "blue"
  , white_char               = '●'
  , white_color              = "green"
  , empty_intersection_char  = '·'
  , empty_intersection_color = "grey"
  , horizontal_spacing_char  = ' '
  , horizontal_spacing_color = "grey"
  }

textColorToColor :: String -> IO ()
textColorToColor text
  | text == "red"   = red
  | text == "green" = green
  | text == "blue"  = blue
  | text == "grey"  = grey

instance A.FromJSON Config
instance A.ToJSON Config

decodeConfig       :: BLU.ByteString -> Config
decodeConfig input =  fromMaybe defaultConfig (A.decode input :: Maybe Config)

readConfigFile :: IO BLU.ByteString
readConfigFile =  BL.readFile "src/Configs/config.json"

printConfig :: IO ()
printConfig =  do
  input               <- readConfigFile
  let decodedContents =  decodeConfig input
  print decodedContents
