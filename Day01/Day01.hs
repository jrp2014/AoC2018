module Main where

import qualified Data.Set as Set

-- Specialize read for reading Ints
readInt :: String -> Int
readInt = read

-- cumulative sum of list
accumulate :: [Int] -> [Int]
accumulate = scanl (+) 0

-- This is probably O(N^2) using lists, but is much faster using Sets.
-- This version will not terminate if there are no duplicatess.
firstDuplicate :: Ord a => [a] -> a
firstDuplicate = go Set.empty
  where
    go :: Ord a => Set.Set a  -> [a] -> a
    go s (h : t)
      | h `Set.member` s = h
      | otherwise = go (Set.insert h s) t

main :: IO ()
main = do
  freqChangesS <- readFile "input.txt"
  -- strip out the '+'s because read doesn't like them
  let freqChangesI = map readInt . lines $ filter (/= '+') freqChangesS
  -- Part One
  print $ sum freqChangesI
  -- Part Two
  print . firstDuplicate . accumulate $ cycle freqChangesI
  
