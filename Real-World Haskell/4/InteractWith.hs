import System.Environment (getArgs)

interactWith function inputFile outputFile = do
    input <- readFile inputFile
    writeFile outputFile (function input)

main =  mainWith myFunction
  where mainWith function = do
          args <- getArgs
          case args of
            [input, output] -> interactWith function input output
            _ -> putStrLn "error: exactly two arguments needed"
        -- replace "id" with the name of our function below
        myFunction = id

-- Use `ghc --make InteractWith`
-- Then use:
-- $ ./Interact hello-in.txt hello-out.txt
-- $ cat hello-in.txt
-- hellow world