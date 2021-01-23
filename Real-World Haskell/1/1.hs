main = interact wordCount
  where wordCount input = show (length (lines input)) ++ "\n"

-- 3. Count the words:
-- main = interact wordCount
--   where wordCount input = show (length (words input)) ++ "\n"

-- 4. Count the chars:
-- main = interact wordCount
--   where wordCount input = show (length input) ++ "\n"