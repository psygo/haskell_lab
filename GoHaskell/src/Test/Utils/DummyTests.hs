import Test.Utils.TestInfra

validate :: IO ()
validate  = runTests dummyTests

dummyFunc :: Int -> Int -> Int
dummyFunc a b = a + b

dummyCorrectTest :: Test
dummyCorrectTest  = Test "2 + 2 = 4" (dummyFunc 2 2 == 4)

dummyWrongTest :: Test
dummyWrongTest  = Test "2 + 3 = 4" (dummyFunc 2 3 == 4)

dummyTests :: Tests
dummyTests = Tests [dummyCorrectTest, dummyWrongTest]
