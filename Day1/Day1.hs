module Main where

-- Specialize read for reading Ints
readInt :: String -> Int
readInt = read

-- cumulative sum of list
accumulate :: [Int] -> [Int]
accumulate = scanl (+) 0

-- This is probably O(N^2) but it's not worth using a Set-based
-- version for this problem.  This version will not terminate if there
-- no duplicatess.
-- Could, no doubt be turned into a fold...
firstDuplicate :: Eq a => [a] -> a
firstDuplicate = go []
  where
    go :: Eq a => [a]  -> [a] -> a
    go s (h : t)
      | h `elem` s = h
      | otherwise = go (h:s) t

main :: IO ()
main = do
  freqChangesS <- readFile "input.txt"
  -- strip out the '+'s because read doesn't like them
  let freqChangesI = map readInt . lines $ filter (/= '+') freqChangesS
  -- Part One
  print $ sum freqChangesI
  -- Part Two
  print . firstDuplicate . accumulate $ cycle freqChangesI
  
