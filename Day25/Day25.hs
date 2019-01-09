module Main where

import           Data.Graph.Inductive           ( UGr
                                                , mkUGraph
                                                , noComponents
                                                )

type Point = (Int, Int, Int, Int)

manhattan :: Point -> Point -> Int
manhattan (a, b, c, d) (a', b', c', d') =
  abs (a - a') + abs (b - b') + abs (c - c') + abs (d - d')


mkGraph :: [String] -> UGr
mkGraph input = mkUGraph nodes edges
 where
  nodes  = [1 .. length input]
  lnodes = zip nodes (parse input)
  -- Make edges between close nodes
  edges =
    [ (node1, node2)
    | (node1, x) <- lnodes
    , (node2, y) <- lnodes
    , manhattan x y <= 3
    ]

ex1 :: [String]
ex1 =
  [ "0,0,0,0"
  , "3,0,0,0"
  , "0,3,0,0"
  , "0,0,3,0"
  , "0,0,0,3"
  , "0,0,0,6"
  , "9,0,0,0"
  , "12,0,0,0"
  ]

ex2 :: [String]
ex2 =
  [ "-1,2,2,0"
  , "0,0,2,-2"
  , "0,0,0,-2"
  , "-1,2,0,0"
  , "-2,-2,-2,2"
  , "3,0,2,-1"
  , "-1,3,2,2"
  , "-1,0,-1,0"
  , "0,2,1,-2"
  , "3,0,0,0"
  ]

ex3 :: [String]
ex3 =
  [ "1,-1,0,1"
  , "2,0,-1,0"
  , "3,2,-1,0"
  , "0,0,3,1"
  , "0,0,-1,-1"
  , "2,3,-2,0"
  , "-2,2,0,0"
  , "2,-2,0,-1"
  , "1,-1,0,-1"
  , "3,2,0,2"
  ]

ex4 :: [String]
ex4 =
  [ "1,-1,-1,-2"
  , "-2,-2,0,1"
  , "0,2,1,3"
  , "-2,3,-2,1"
  , "0,2,3,-2"
  , "-1,-1,1,-2"
  , "0,-2,-1,0"
  , "-2,2,3,-1"
  , "1,2,2,0"
  , "-1,-2,0,-2"
  ]

solve1 :: [String] -> Int
solve1 = noComponents . mkGraph

parse :: [String] -> [Point]
parse = map parseLine where parseLine s = read $ '(' : s ++ ")"

main :: IO ()
main = do
  print $ solve1 ex1
  print $ solve1 ex2
  print $ solve1 ex3
  print $ solve1 ex4
  -- Part 1
  input <- readFile "input.txt"
  let linput = lines input
  print $ solve1 linput
