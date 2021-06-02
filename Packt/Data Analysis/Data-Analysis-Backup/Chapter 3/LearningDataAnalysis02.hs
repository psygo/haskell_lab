{-|
Module      : LearningDataAnalysis02
Description : Contains functions for the conversion of
              a csv file into an SQLite3 databse.
Maintainer  : jcchurch@gmail.com

To install the HDBC Sqlite3 library using Debian Linux, use this command:

sudo apt-get install libghc-hdbc-sqlite3-dev libsqlite3-dev

To install the necessary libraries for Chapter 2, use this cabal command:

cabal install csv HDBC sqlite HDBC-sqlite3

If HDBC-sqlite3 doesn't work on your system, try HSQL-sqlite3:

sudo apt-get install libghc-hsql-sqlite3-dev

cabal install csv HSQL-sqlite3

All GHCi examples used in this chapter:

:l LearningDataAnalysis02
let falconsScores = [17,31,23,23,28,31,13,10,10,28,13,34,21,27,24,20]
let sumOfFalconsScores = sum falconsScores
sumOfFalconsScores
let numberOfFalconsGames = length falconsScores
numberOfFalconsGames 
let meanScoreofFalcons = sumOfFalconsScores / numberOfFalconsGames 
let meanFalconsScore = (realToFrac sumOfFalconsScores) / (fromIntegral numberOfFalconsGames)
meanFalconsScore 
average [1, 2, 3]
average [1, 2, 3.5] 
average [1.5, 2.5, 3.5] 
let a = [1, 2, 3] 
average a 
average []
import Data.List 
csv <- parseCSVFromFile "all_week.csv"
either (\error -> Left "Problem Reading File") (\csv -> getColumnInCSV csv "mag") csv
either (\error -> Left "Problem Reading File") (\csv -> getColumnInCSV csv "not a column") csv 
csv <- parseCSVFromFile "all_week.csv" 
either (\error -> Left "Problem Reading File") (\csv -> applyToColumnInCSV (average . readColumn) csv "mag") csv
either (\error -> Left "Problem Reading File") (\csv -> applyToColumnInCSV (average . readColumn) csv "not a column") csv 
applyToColumnInCSVFile (average . readColumn) "all_week.csv" "mag"
applyToColumnInCSVFile (maximum . readColumn) "all_week.csv" "mag" 
applyToColumnInCSVFile (minimum . readColumn) "all_week.csv" "mag" 
convertCSVFileToSQL "all_week.csv" "earthquakes.sql" "oneWeek" ["time TEXT", "latitude REAL", "longitude REAL", "depth REAL", "mag REAL", "magType TEXT", "nst INTEGER", "gap REAL", "dmin REAL", "rms REAL", "net REAL", "id TEXT", "updated TEXT", "place TEXT", "type TEXT"]
conn <- connectSqlite3 "earthquakes.sql" 
magnitudes <- quickQuery' conn "SELECT mag FROM oneWeek" [] 
fromSql $ head $ head magnitudes :: Double 
let magnitudesDouble = map (\record -> fromSql $  head record :: Double)   magnitudes 
average magnitudesDouble

-}

module LearningDataAnalysis02 where

import System.IO
import Data.List
import Data.Either
import Text.CSV
import Database.HDBC.Sqlite3
import Database.HDBC

{-|
   Compute the average of a list of values
   Returns a fractional value.
-}
average :: (Real a, Fractional b) => [a] -> b
average xs = realToFrac(sum xs) / fromIntegral(length xs)

{-|
   Converts a list of String values into a list of Double values
   Used to read columns from a csv file.
   Returns a list of Double values.
-}
readColumn :: [String] -> [Double]
readColumn = map read

{-|
   Opens a CSV file and identifies which column index
   belongs to a specified string.
   Returns (Left errorMessage) or (Right index)
-}
getColumnInCSVFile ::
       FilePath
    -> String
    -> IO (Either String Integer)
getColumnInCSVFile
    inFileName
    column = do
      -- Open and read the CSV file
      input <- readFile inFileName
      let records = parseCSV inFileName input

      -- Check to make sure this is a good csv file
      -- If good, go to getColumnInCSV for the column index
      return $ either
             (\_ -> Left "This does not appear to be a CSV file")
             (\csv -> getColumnInCSV csv column)
             records

{-|
   Gets a column from a CSV value.
   Returns (Left errorMessage) or (Right index)
-}
getColumnInCSV :: CSV -> String -> Either String Integer
getColumnInCSV csv column =
      case lookupResponse of
          Nothing -> Left "The column does not exist in this CSV file."
          Just x -> Right (fromIntegral x)
  where
      -- This line does the lookup to see if column is in our CSV
      lookupResponse = findIndex (== column) (head csv)

{-|
   Applies a function of a column in a csv file specified
   by a String and FilePath.
   Returns IO (Left errorMessage) or IO (Right b)
-}
applyToColumnInCSVFile ::
      ([String] -> b)
   -> FilePath
   -> String
   -> IO (Either String b)
applyToColumnInCSVFile
    func
    inFileName
    column = do
      -- Open and read the CSV file
      input <- readFile inFileName
      let records = parseCSV inFileName input

      -- Check to make sure this is a good csv file
      return $ either
        handleCSVError
        (\csv -> applyToColumnInCSV func csv column)
        records 
  where
      handleCSVError _ = Left "This does not appear to be a CSV file."

{-|
   Applies a function to a column (specified by a Sring) in a CSV value
   Returns (Left errorMessage) or (Right b)
-}
applyToColumnInCSV ::
       ([String] -> b)
    -> CSV
    -> String
    -> Either String b
applyToColumnInCSV
    func
    csv
    column =
      either
        Left
        (Right . func . elements)
        columnIndex
  where
      columnIndex = getColumnInCSV csv column
      nfieldsInFile = length $ head csv
      records = tail $ filter (\record -> nfieldsInFile == length record) csv
      elements ci = map (\record -> genericIndex record ci) records

{-|
   Converts a CSV file to an SQL database file.
   Prints "Successful" if successful, or prints error message otherwise.
-}
convertCSVFileToSQL ::
       FilePath
    -> FilePath
    -> String
    -> [String]
    -> IO ()
convertCSVFileToSQL
    inFileName
    outFileName
    tableName
    fields = do
      -- Open and read the CSV file
      input <- readFile inFileName
      let records = parseCSV inFileName input

      -- Check to make sure this is a good csv file
      either handleCSVError convertTool records 
  where
      convertTool = convertCSVtoSQL tableName outFileName fields
      handleCSVError _ = putStrLn "This does not appear to be a CSV file."

{-|
   Converts a CSV string into an SQL database
   Prints "Successful" if successful, or prints error message otherwise.
-}
convertCSVtoSQL ::
       String
    -> FilePath
    -> [String]
    -> CSV
    -> IO ()
convertCSVtoSQL
    tableName
    outFileName
    fields
    records = 

      -- Check to make sure that the number of
      --   columns matches the number of fields
      if nfieldsInFile == nfieldsInFields then do
        -- Open a connection
        conn <- connectSqlite3 outFileName

        -- Create a new table 
        run conn createStatement []

        -- Load contents of CSV file into table
        stmt <- prepare conn insertStatement
        executeMany stmt $ tail $ filter (\record -> nfieldsInFile == length record) sqlRecords

        -- Commit changes
        commit conn

        -- Close the connection
        disconnect conn
        
        -- Report that we were successful
        putStrLn "Successful"
       else
        putStrLn "The are a different number of fields specified from the fields in the CSV file"
  where
      nfieldsInFile = length $ head records
      nfieldsInFields = length fields
      createStatement = "CREATE TABLE " ++ tableName ++ " (" ++ intercalate ", " fields ++ ")"
      insertStatement = "INSERT INTO " ++ tableName ++ " VALUES (" ++ intercalate ", " (replicate nfieldsInFile "?") ++ ")"
      sqlRecords = map (map toSql) records
