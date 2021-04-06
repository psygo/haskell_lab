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
                                           grey

printTestsWithIndices :: [(Int,Test)] -> IO ()
printTestsWithIndices []         = return ()
printTestsWithIndices ((i,t):ts) = do
                                      putStrLn dashes
                                      putStrLn $ "#" ++ (show i)
                                      testResultPrinter t
                                      printTestsWithIndices ts

testsResults :: [Test] -> IO ()
testsResults []    = return ()
testsResults tests = do
                        putStrLn emptyPadding
                        let indexedTests  = zip [1..] tests
                        printTestsWithIndices indexedTests
                        putStrLn dashes
                        blue
                        let totalLength   = length tests
                        putStrLn $ "Total Tests:   " ++ (show totalLength)
                        let failures      = filterFailures tests
                        let totalFailures = length failures
                        let totalPassing  = totalLength - totalFailures
                        green
                        putStrLn $ "Passing Tests: " ++ (show totalPassing)
                        red
                        putStrLn $ "Failed Tests:  " ++ (show totalFailures)
                        grey
                        putStrLn dashes
                        putStrLn emptyPadding

filterFailures :: [Test] -> [Test]
filterFailures [] = []
filterFailures ((Test title result):ts) = if result
                                             then (filterFailures ts)
                                             else (Test title result):(filterFailures ts)

main :: IO ()
main  = testsResults dummyTests
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

blue, green, red, grey :: IO ()
blue  = setSGR [SetColor Foreground Vivid Blue ]
green = setSGR [SetColor Foreground Vivid Green]
red   = setSGR [SetColor Foreground Vivid Red  ]
grey  = setSGR [SetColor Foreground Dull  White]

emptyLine :: String
emptyLine = ""

emptyPadding :: String
emptyPadding = "\n"
--------------------------------------------------------------------------------
