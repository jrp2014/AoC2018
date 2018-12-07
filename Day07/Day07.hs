module Main where

import Data.List (nub, sort)
import Graph

type Rule = Graph Char

parse :: [String] -> [(Char, Char)]
parse = map (parseLine . words)
  where
    parseLine [_, [p], _, _, _, _, _, [s], _, _] = (p, s)

toRule :: [(Char, Char)] -> Rule
toRule rs = Graph verts rs
  where
    verts = sort $ nub $ map fst rs ++ map snd rs

ex1 :: [String]
ex1 =
  [ "Step C must be finished before step A can begin."
  , "Step C must be finished before step F can begin."
  , "Step A must be finished before step B can begin."
  , "Step A must be finished before step D can begin."
  , "Step B must be finished before step E can begin."
  , "Step D must be finished before step E can begin."
  , "Step F must be finished before step E can begin."
  ]

solve1 :: [(Char, Char)] -> String
solve1 = tsort . toRule

main :: IO ()
main = do
  rules <- readFile "input.txt"
  let prules = parse $ lines rules
  print $ parse ex1
  print $ solve1 $ parse ex1
  -- Part One
  print $ solve1 prules
  -- Part Two
--  print $ solve2 ex1
--  print $ solve2 lrules
