module Main where

import Data.List
import Data.List.Unique

solve1 :: [String] -> Int
solve1 = sumTuples . map (foldl incCount (0, 0) . occurrences)

incCount :: (Int, Int) -> (Int, [a]) -> (Int, Int)
incCount (twos, threes) (2, _) = (twos + 1, threes)
incCount (twos, threes) (3, _) = (twos, threes + 1)
incCount x _ = x

sumTuples :: [(Int, Int)] -> Int
sumTuples xs = sum twos * sum threes
  where
    (twos, threes) = unzip xs

ex1 :: [String]
ex1 = ["abcdef", "bababc", "abbcde", "abcccd", "aabcdd", "abcdee", "ababab"]

-- Part 2
--
-- This is a more general solution than is required if we
-- guarantee that exactly one pair of IDs differ by only 1 char
--
-- Given a list of box IDs produce a list of box ID pairs
-- that can be checked for closeness
-- This version avoid duplicates
combinations :: [a] -> [(a, a)]
combinations l = concatMap (zip l) (tails $ tail l)
-- combinations l = [ (diffCount x y, (x, y)) | x <- l, y <- l ]

diffCount :: Eq a => [a] -> [a] -> Int
diffCount [] [] = 0
diffCount (x:xs) (y:ys)
  | x /= y = 1 + diffCount xs ys
  | otherwise = diffCount xs ys

-- capture the number of differences between a pair of IDs
diffs :: Eq a => [[a]] -> [(Int, ([a], [a]))]
diffs l = map (\z@(x, y) -> (diffCount x y, z)) $ combinations l

-- bestMatch ... one with the fewst diffs wins
bestMatch :: Eq a => [[a]] -> [(Int, ([a], [a]))]
bestMatch l = sortOn fst (diffs l)
-- bestMatch l = filter ((==1) . fst) (diffs l)

commonLetters :: Eq a => [a] -> [a] -> [a]
commonLetters (x:xs) (y:ys)
  | x == y = x : commonLetters xs ys
  | otherwise = commonLetters xs ys
commonLetters [] [] = []

solve2 :: Eq a => [[a]] -> [a]
solve2 l = commonLetters x y
  where
    (_, (x, y)) = head $ bestMatch l

ex2 :: [String]
ex2 = ["abcde", "fghij", "klmno", "pqrst", "fguij", "axcye", "wvxyz"]

main :: IO ()
main = do
  ids <- readFile "input.txt"
  let lids = lines ids
  -- Part One
  print $ solve1 ex1
  print $ solve1 lids
  -- Part Two
  print $ solve2 ex2
  print $ solve2 lids
