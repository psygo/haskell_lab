--------------------------------------------------------------------------------
import           System.Console.ANSI
--------------------------------------------------------------------------------
-- Test Harness

testResultPrinter :: Test -> IO ()
testResultPrinter (Test title result) = do
                                           blue
                                           putStrLn title
                                           if result then green else red
                                           putStrLn (show result)
                                           white

testsResults :: [Test] -> IO ()
testsResults [] = return ()
testsResults tests = do
                         let indexedTests = zip [1..] tests
                         printTests indexedTests
                         putStrLn dashes
                           where
                             printTests :: [(Int,Test)] -> IO ()
                             printTests [] = return ()
                             printTests ((i,t):ts) = do
                                                        putStrLn dashes
                                                        putStrLn $ "#" ++ (show i)
                                                        testResultPrinter t
                                                        printTests ts
--------------------------------------------------------------------------------
-- Dummy Tests

data Test = Test String Bool
            deriving Show

dummyFunc :: Int -> Int -> Int
dummyFunc a b = a + b

dummyCorrectTest :: Test
dummyCorrectTest  = Test "2 + 2 = 4" ((dummyFunc 2 2) == 4)

dummyWrongTest :: Test
dummyWrongTest  = Test "2 + 3 = 4" ((dummyFunc 2 3) == 4)

dummyTests :: [Test]
dummyTests = [dummyCorrectTest, dummyWrongTest]
--------------------------------------------------------------------------------
-- Formatting Helpers

dashes :: String
dashes =  replicate 80 '-'

blue, green, red, white :: IO ()
blue  = setSGR [SetColor Foreground Vivid Blue ]
green = setSGR [SetColor Foreground Vivid Green]
red   = setSGR [SetColor Foreground Vivid Red  ]
white = setSGR [SetColor Foreground Dull  White]
--------------------------------------------------------------------------------
