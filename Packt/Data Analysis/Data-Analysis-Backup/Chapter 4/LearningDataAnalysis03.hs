{-|
Module      : LearningDataAnalysis03
Description : Contains functions that allow matching on individual
              fields found in csv files for the purpose of cleaning
              datasets.
Maintainer  : jcchurch@gmail.com

To install the regular expression posix library, use this cabal command:

cabal install regex-posix

All GHCi examples used in this chapter:

:l LearningDataAnalysis03
csv <- parseCSVFromFile "poorFieldCounts.csv"
either Left (\csv -> Right $ countFieldsInEachRecord csv) csv
either Left (\csv -> Right $ lineNumbersWithIncorrectCount csv) csv
csv <- parseCSVFromFile "all_week.csv"
either Left (\csv -> Right $ lineNumbersWithIncorrectCount csv) csv
identifyMatchingFields (\x -> x =~ "") ["1", "Clark Kent", "Journalist", "Smallville"] ["Id", "Name", "Profession", "Location"] 0
identifyMatchingFields (\x -> x =~ "Journ") ["1", "Clark Kent", "Journalist", "Metropolis"] ["Id", "Name", "Profession", "Location"] 0
identifyMatchingFields (\x -> x =~ "Hero") ["1", "Clark Kent", "Journalist", "Metropolis"] ["Id", "Name", "Profession", "Location"] 0
csv <- parseCSVFromFile "poordata.csv"
either (\error -> Left "CSV Problem") (\csv -> identifyInCSV (\x -> x =~ "PA") csv "Number") csv
identifyInCSVFile (\x -> x =~ "^\\s*$") "poordata.csv" "Number"
identifyInCSVFileFromColumn  (\x -> not (x =~ "^[1-9][0-9]?/[1-9][0-9]?/1[0-9][0-9][0-9]$")) "poordata.csv" "Number" "Birthday"

-}

module LearningDataAnalysis03 where

import Text.CSV
import Data.List
import Text.Regex.Posix ((=~))
import LearningDataAnalysis02

{-|
  For each line in a CSV value, report the number of elements on each line.
-}
countFieldsInEachRecord :: CSV -> [Integer]
countFieldsInEachRecord csv = map genericLength (init csv)

{-|
  Reports which lines in a CSV value have a different number of
  elements (and how many) from the number of elements on the first line.
-}
lineNumbersWithIncorrectCount :: CSV -> [(Integer, Integer)]
lineNumbersWithIncorrectCount (fields:csv) =
      filter (\(_, thisCount) -> thisCount /= nfields) lineNoCountPairs
  where
      nfields = genericLength fields
      count = countFieldsInEachRecord csv
      lineNoCountPairs = zip [1..] count 

{-|
  Based on a predicate function (String -> Bool), returns a
  tuple (String, String, String) for each field in a single record
  that matches the predicate function. The returned tuple represents:
  1. The primary index of the row.
  2. The column header.
  3. The contents of the field itself.

  The parameters are:
  1. myFieldFunc: the predicate function
  2. record: A single record within a CSV value
  3. headings: The headings of this CSV value
  4. idColumnIndex: The numberical index of the primary index column.
-}
identifyMatchingFields ::
      (String -> Bool)
  -> [String]
  -> [String]
  -> Integer
  -> [(String, String, String)]
identifyMatchingFields
  myRegexFunc
  record
  headings
  idColumnIndex =
      filter (\(_, _, field) -> myRegexFunc field) keyvalue
  where
    nfields = genericLength headings
    keyvalue = zip3
                    (replicate nfields (genericIndex record idColumnIndex))
                    headings
                    record
 
{-|
  Based on a predicate function (String -> Bool), returns a
  tuple (String, String, String) for each field in a CSV value
  that matches the predicate function. The returned tuple represents
  1. The primary index of the row.
  2. The column header.
  3. The contents of the field itself.

  The parameters are:
  1. myFieldFunc: the predicate function
  2. csv: The CSV value.
  3. idColumn: The column name for the primary identifier column.
-}
identifyInCSV ::
       (String -> Bool)
    -> CSV
    -> String
    -> Either String [(String, String, String)]
identifyInCSV
    myFieldFunc
    csv
    idColumn =
      either
        Left
        (\ci -> Right $ concatMap
                         (\record -> identifyMatchingFields myFieldFunc record (head csv) ci)
                         (tail csv)
        )
        columnIndex
  where
      columnIndex = getColumnInCSV csv idColumn

{-|
  Based on a predicate function (String -> Bool), returns
  either IO (Left errorMessage) or IO (Right (String, String, String)) for
  each field in a CSV value that matches the predicate function.
  The returned tuple represents
  1. The primary index of the row.
  2. The column header.
  3. The contents of the field itself.

  The parameters are:
  1. myFieldFunc: the predicate function
  2. inFileName: The csv filename
  3. idColumn: The column name for the primary identifier column.
-}
identifyInCSVFile ::
       (String -> Bool)
    -> FilePath
    -> String
    -> IO (Either String [(String, String, String)])
identifyInCSVFile
    myRegexFunc
    inFileName
    idColumn = do
      records <- parseCSVFromFile inFileName
      return $ either
               (\err -> Left "This does not appear to be a CSV file")
               (\csv -> identifyInCSV myRegexFunc (init csv) idColumn)
               records

{-|
  Based on a predicate function (String -> Bool), returns
  either IO (Left errorMessage) or IO (Right (String, String, String)) for
  each field in a CSV value that matches the predicate function and matches
  a desired column name.
  The returned tuple represents
  1. The primary index of the row.
  2. The column header.
  3. The contents of the field itself.

  The parameters are:
  1. myFieldFunc: the predicate function
  2. inFileName: The csv filename
  3. idColumn: The column name for the primary identifier column.
  4. desiredHeading: The name of the desired column
-}
identifyInCSVFileFromColumn ::
       (String -> Bool)
    -> FilePath
    -> String
    -> String
    -> IO (Either String [(String, String, String)])
identifyInCSVFileFromColumn
    myRegexFunc
    inFileName
    idColumn
    desiredHeading = do
        allFields <- identifyInCSVFile myRegexFunc inFileName idColumn
        return $ either
            Left
            (Right . filter (\(_, heading, _) -> heading == desiredHeading))
            allFields
