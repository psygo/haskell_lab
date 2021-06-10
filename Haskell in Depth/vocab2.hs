import qualified Data.Text as T
import qualified Data.Text.IO as TIO

type Entry = (T.Text, Int)
type Vocabulary = [Entry]

extractVocab :: T.Text -> Vocabulary
extractVocab t = map buildEntry $ group $ sort ws
  where
    ws = map T.toCaseFold $ filter (not . T.null) $ map cleanWord $ T.words t
    buildEntry xs@(x:_) = (x, length xs)
    cleanWord = T.dropAround (not . isLetter)

printAllWords :: Vocabulary -> IO ()
printAllWords vocab = do
  putStrLn "All words: "
  TIO.putStrLn $ T.unlines $ map fst vocab

processTextFile :: FilePath -> IO ()
processTextFile fname = do
  text <- TIO.readFile fname
  let vocab = extractVocab text
  printAllWords vocab

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fname] -> processTextFile fname
    _ -> putStrLn "Usage: vocab-builder filename"
