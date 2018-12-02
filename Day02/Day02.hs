module Main where

import Data.List.Unique

solve1 :: [String] -> Int
solve1 =  sumTuples . map (foldl incCount (0, 0) . occurrences)

incCount :: (Int, Int) -> (Int, [a]) -> (Int, Int)
incCount (twos, threes) (2, _) = (twos + 1, threes)
incCount (twos, threes) (3, _) = (twos, threes + 1)
incCount x _  = x

sumTuples :: [(Int, Int)] -> Int
sumTuples xs = twos * threes
  where
    twos = sum $ map fst xs
    threes = sum $ map snd  xs


ex1 :: [String]
ex1 = [
  "abcdef",
  "bababc",
  "abbcde",
  "abcccd",
  "aabcdd",
  "abcdee",
  "ababab"
  ]

main :: IO ()
main = do
  ids <- readFile "input.txt"
  -- Part One
  print $ solve1 ex1
  print $ solve1 (lines ids)
  -- Part Two
  
