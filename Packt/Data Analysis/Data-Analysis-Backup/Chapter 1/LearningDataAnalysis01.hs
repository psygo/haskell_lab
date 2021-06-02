{-|
Module      : LearningDataAnalysis01
Description : Contains a median and a vowelIndices function.
Maintainer  : jcchurch@gmail.com

All GHCi examples used in this chapter:

:l LearningDataAnalysis01
2 + 2
(+) 2 2
2 + 4 * 5
(+) 2 $ (*) 4 5
let word = "apple"
zip [1..] word
filter (\(_, letter) -> elem letter "aeiouAEIOU") $ zip [1..] word
:t \(_, letter) -> elem letter "aeiouAEIOU"
map fst . filter (\(_, letter) -> elem letter "aeiouAEIOU") $ zip [1..] word
:t fst
:t snd
fst (1, 'a')
snd (1, 'a')
vowelIndices "apple"
vowelIndices "orange"
vowelIndices "grapes"
vowelIndices "supercalifragilisticexpialidocious"
vowelIndices "why"
median . map fromIntegral $ vowelIndices "supercalifragilisticexpialidocious"
-}

module LearningDataAnalysis01 where
import Data.List

{-|
    Computes the median of a list of values.
    If the list is empty, 0 is returned.
-}
median :: [Double] -> Double
median [] = 0
median xs = if oddInLength then
                middleValue
                 else
                (middleValue + beforeMiddleValue) / 2

  where
      sortedList = sort xs
      oddInLength = 1 == mod (genericLength xs) 2
      middle = floor $ genericLength xs / 2
      middleValue = genericIndex sortedList middle
      beforeMiddleValue = genericIndex sortedList (middle-1)

{-|
    Returns a list of integers indicating the index positions
    of vowels in a string.
-}
vowelIndices :: String -> [Integer]
vowelIndices word = map fst . filter
                    (\(_, letter) -> elem letter "aeiouAEIOU") $ zip [1..] word

