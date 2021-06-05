{-# LANGUAGE OverloadedStrings #-}

module Main where

import Brick

ui :: Widget ()
ui = str "Hello, World!"

main :: IO ()
main = tui
main = simpleMain ui
