module Test.Utils.TestUtils where

import System.Console.ANSI

dashes :: String
dashes =  replicate 80 '-'

blue, green, red, grey :: IO ()
blue  = setSGR [SetColor Foreground Vivid Blue ]
green = setSGR [SetColor Foreground Vivid Green]
red   = setSGR [SetColor Foreground Vivid Red  ]
grey  = setSGR [SetColor Foreground Dull  White]

emptyLine :: String
emptyLine = ""

emptyPadding :: String
emptyPadding = "\n"
