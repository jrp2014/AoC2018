module Main where

import Control.Monad (replicateM)
import Data.Maybe (mapMaybe)
import qualified Text.Parsec as P

-- Parse the input to a list of Int
-- This is necessary to drive the parse
type Summed = P.Parsec [Int] () Int

ex1 :: String
ex1 = "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"

-- sm 
solve :: Summed -> [Int] -> Int
solve sm xs =
  case P.parse sm "" xs of
    Left _ -> error "parse failed"
    Right y -> y

solve1 :: [Int] -> Int
solve1 = solve sum1

solve2 :: [Int] -> Int
solve2 = solve sum2

sum1 :: Summed
sum1 = do
  qcn <- P.anyToken
  qme <- P.anyToken
  sumChildren <- sum <$> replicateM qcn sum1
  sumMeta <- sum <$> replicateM qme P.anyToken
  return $ sumChildren + sumMeta

sum2 :: Summed
sum2 = do
  qcn <- P.anyToken
  qme <- P.anyToken
  children <- replicateM qcn sum2
  metas <- replicateM qme P.anyToken
  return $
    if null children
      then sum metas
      -- The metadata entries are indexes which refer to child nodes
      -- ... or could just use (!!) and filter out the invalid indices
      else sum . mapMaybe (safeIndex children) $ metas

-- Indexation from 1, rather than 0
safeIndex :: [a] -> Int -> Maybe a
safeIndex (x:_) 1 = Just x
safeIndex [] _ = Nothing
safeIndex (_:xs) i
  | i < 1 = Nothing
  | otherwise = safeIndex xs (i - 1)

parse :: String -> [Int]
parse = map read . words

main :: IO ()
main = do
  tree <- readFile "input.txt"
  let ptree = parse tree
  print $ words ex1
  -- Part One
  print $ solve1 $ parse ex1
  print $ solve1 ptree
  -- Part Two
  print $ solve2 $ parse ex1
  print $ solve2 ptree
