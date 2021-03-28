import System.IO (stdin, stdout, hGetLine, hPutStrLn, hPrint)

getLine = hGetLine stdin

putStrLn = hPutStrLn stdout

-- print :: a -> IO ()
-- print = hPrint stdout