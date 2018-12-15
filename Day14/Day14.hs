module Main where

import           Data.Char     (digitToInt)
import           Data.List     (isPrefixOf, tails)
import qualified Data.Sequence as S

type Score = Int

type Index = Int

data Recipes =
  Recipes Index
          Index
          (S.Seq Score)
  deriving (Show, Eq)

-- lazy list of chocolate practice recipe numbers
newRecipes :: [Score]
newRecipes = 3 : 7 : newRecipes' (Recipes 0 1 (S.fromList [3, 7]))
  where
    newRecipes' :: Recipes -> [Score]
    newRecipes' (Recipes elf1 elf2 scores) =
      newDigits ++ newRecipes' (Recipes elf1' elf2' scores')
      where
        scores' = scores S.>< S.fromList newDigits
        score1 = scores `S.index` elf1
        score2 = scores `S.index` elf2
        newDigits = toDigits (score1 + score2)
        length' = S.length scores'
        elf1' = (elf1 + 1 + score1) `mod` length'
        elf2' = (elf2 + 1 + score2) `mod` length'

toDigits :: Int -> [Int]
toDigits = map digitToInt . show

input :: Int
input = 440231

solve1 :: Int -> String
solve1 n = concatMap show $ take 10 $ drop n newRecipes

-- TODO:: This works, but is slow. Find a faster algorithm.
solve2 :: Int -> Int
solve2 n =
  length . takeWhile (not . (toDigits n `isPrefixOf`)) . tails $ newRecipes

main :: IO ()
main = do
  print $ solve1 9
  print $ solve1 5
  print $ solve1 18
  print $ solve1 2018
  -- Part 1
  print $ solve1 input
  -- Part 2
  print $ solve2 input
