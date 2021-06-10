module Utils.Utils where

import System.Console.ANSI

red, green, blue, grey :: IO ()
red   = setSGR [SetColor Foreground Vivid Red  ]
green = setSGR [SetColor Foreground Vivid Green]
blue  = setSGR [SetColor Foreground Vivid Blue ]
grey  = setSGR [SetColor Foreground Dull  White]

alphabetCoordinates :: Int -> String
alphabetCoordinates i = concat [[char, ' '] | char <- alphabet, char /= 'I']
  where
    alphabet :: String
    alphabet = take (if i < 9 then i else i+1) ['A'..'Z']
