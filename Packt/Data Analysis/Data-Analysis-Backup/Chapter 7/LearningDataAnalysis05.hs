{-
Module      : LearningDataAnalysis05
Description : Contains functions for performing simple hypothesis testing.
Maintainer  : jcchurch@gmail.com

To install the erf and exact-combinatorics libraries, use this cabal command:

cabal install erf
cabal install exact-combinatorics

All GHCi examples used in this chapter:

:l LearningDataAnalysis02 LearningDataAnalysis04 LearningDataAnalysis05
:m LearningDataAnalysis02 LearningDataAnalysis04 LearningDataAnalysis05
import Graphics.EasyPlot
plot (PNG "coinflips.png") $ Function2D [Title "Coin Flip Probabilities"] [Range 0 1000] (\k -> probabilityMassFunction (floor k) 1000 0.5)
probabilityMassFunction 500 1000 0.5
sum $ map (\k -> probabilityMassFunction k 1000 0.5) [0..1000]
sum $ map (\k -> probabilityMassFunction k 1000 0.5) [(500-40)..(500+40)]
import System.Random
g <- newStdGen
random g :: (Double, StdGen)
take 3 $ randoms g :: [Double]
take 5 $ randomRs (0,100) g
let coinflips = take 1000 $ randomRs (0, 1) g
sum coinflips
queryDatabase "winloss.sql" "SELECT SUM(awayscore), sum(homescore) FROM winloss"
runsAtHome <- queryDatabase "winloss.sql" "SELECT hometeam, SUM(homescore) FROM winloss GROUP BY hometeam ORDER BY hometeam"
runsAtHome
runsAway <- queryDatabase "winloss.sql" "SELECT awayteam, sum(awayscore) FROM winloss GROUP BY awayteam ORDER BY awayteam"
runsAway
let runsHomeAway = zip (readDoubleColumn runsAtHome 1) (readDoubleColumn runsAway 1)
runsHomeAway
import Graphics.EasyPlot
plot (PNG "HomeScoreAwayScore.png") $ Data2D [Title "Runs at Home (x axis) and Runs Away (y axis)"] [] runsHomeAway
let runsHomeAwayDiff = map (\(a,b) -> a-b) runsHomeAway
plot (PNG "HomeScoreAwayScoreDiff.png") $ Data2D [Title "Difference in Runs at Home and Runs Away"] [] $ zip [1..] runsHomeAwayDiff
average runsHomeAwayDiff 
standardDeviation runsHomeAwayDiff / (sqrt $ genericLength runsHomeAwayDiff)
plot (PNG "standardNormal.png") $ Function2D [Title "Standard Normal"] [Range (-4) 4] (\x -> exp(-(x*x)/2)/sqrt(2*pi)) 
import Data.Number.Erf
normcdf 0
1 - (normcdf 0)
invnormcdf 0.5
invnormcdf 1
(1-0.95)/2
invnormcdf 0.025
normcdf 1.96 - 0.025

These functions plot basic Error Functions.

plot (PNG "erf.png") $ Function2D [Title "erf"] [Range (-3) 3] erf
plot (PNG "erfc.png") $ Function2D [Title "erfc"] [Range (-3) 3] erfc
plot (PNG "inverf.png") $ Function2D [Title "inverf"] [Range (-3) 3] inverf
plot (PNG "normcdf.png") $ Function2D [Title "normcdf"] [Range (-1) 1] normcdf 

Plot the CDF of our coin flip problem

plot (PNG "binomialcdf.png") $ Function2D [Title "binomial cdf"] [Range 0 1000] (\x -> 0.5*(1+erf((x-500)/22.3607)))

Coin Flip Histogram

plot (PNG "coinflips.png") $ Function2D [Title "Coin Flip Probabilities"] [Range 0 1000] (\k -> probabilityMassFunction k 1000 0.5)

-}

module LearningDataAnalysis05 where

import Data.List
import Math.Combinatorics.Exact.Binomial
import LearningDataAnalysis02

{-|
    Given the values k, n, and p, computes
    the probability mass of the binomial distribution.
-}
probabilityMassFunction :: Integral a => a -> a -> Double -> Double
probabilityMassFunction k n p =
      fromIntegral (n `choose` k) * (p^k) * ((1-p)^(n-k))

{-|
    Given a list of values, computes the standard deviation
    of those values.
-}
standardDeviation :: [Double] -> Double
standardDeviation values = 
      (sqrt . sum $ map (\x -> (x-mu)*(x-mu)) values) / sqrt_nm1
  where
      mu = average values
      sqrt_nm1 = sqrt (genericLength values - 1)
