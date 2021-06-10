--------------------------------------------------------------------------------
module Test.Utils.TestInfra where

import Test.Utils.TestUtils
--------------------------------------------------------------------------------
-- Data Types

data Test = Test String Bool
            deriving Show

newtype Tests = Tests [Test] deriving Show
newtype IndexedTest = IndexedTest (Int, Test) deriving Show
newtype IndexedTests = IndexedTests [IndexedTest] deriving Show
--------------------------------------------------------------------------------
runTests :: Tests -> IO ()
runTests (Tests []) = return ()
runTests tests      = do
  putStrLn emptyPadding
  printTestsWithIndices (indexTests tests)
  putStrLn dashes
  printReport tests
  grey
  putStrLn dashes
  putStrLn emptyPadding
--------------------------------------------------------------------------------
printTestResult :: Test -> IO ()
printTestResult (Test title result) = do
  blue
  putStrLn title
  if result then green else red
  putStrLn (if result then "✓" else "✖")

printTestsWithIndices :: IndexedTests -> IO ()
printTestsWithIndices (IndexedTests []) = return ()
printTestsWithIndices (IndexedTests ((IndexedTest (index,test)):tests)) = do
  putStrLn dashes
  putStrLn $ "#" ++ show index
  printTestResult test
  grey
  printTestsWithIndices (IndexedTests tests)

indexTests :: Tests -> IndexedTests
indexTests (Tests []) = IndexedTests []
indexTests (Tests tests) = IndexedTests (map indexTest (zip [1..] tests))
                             where
                               indexTest :: (Int,Test) -> IndexedTest
                               indexTest (index,test) = IndexedTest (index,test)

printReport :: Tests -> IO ()
printReport tests = do
  blue
  let totalLength   = totalTests tests
  putStrLn $ "Total Tests:   " ++ show totalLength
  let failures      = filterFailures tests
  let totalFailures = totalTests failures
  let totalPassing  = totalLength - totalFailures
  green
  putStrLn $ "Passing Tests: " ++ show totalPassing
  red
  putStrLn $ "Failed Tests:  " ++ show totalFailures

totalTests :: Tests -> Int
totalTests (Tests tests) = length tests

filterFailures :: Tests -> Tests
filterFailures (Tests []) = Tests []
filterFailures (Tests ((Test title result):ts)) =
  if result
     then filterFailures (Tests ts)
     else prependTest (Test title result) (filterFailures (Tests ts))
       where
         prependTest :: Test -> Tests -> Tests
         prependTest test (Tests tests) = Tests (test:tests)
--------------------------------------------------------------------------------
