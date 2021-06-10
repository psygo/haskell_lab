module Test.BoardTests where

import Test.Utils.TestInfra

import Models.Models
import UI.UI
import Utils.Utils

main :: IO ()
main =  runTests tests

tests :: Tests
tests =  Tests
  [
    ternaryToIntersectionTest
  , alphabetTestLessThanI
  , alphabetTestEqualToI
  ]

ternaryToIntersectionTest :: Test
ternaryToIntersectionTest =  Test
  "Ternary Matrix to Intersection Matrix"
  (ternaryToIntersectionMatrix [[0,1],[1,2]] == [[Empty, Black],[Black,White]])

alphabetTestLessThanI :: Test
alphabetTestLessThanI =  Test
  "Alphabet Coordinates with fewer coordinates than I"
  (alphabetCoordinates 8  == "A B C D E F G H ")

alphabetTestEqualToI :: Test
alphabetTestEqualToI =  Test
  "Alphabet Coordinates skips the letter I"
  (alphabetCoordinates 9  == "A B C D E F G H J ")
