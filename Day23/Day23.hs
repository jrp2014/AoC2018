module Main where

import           Data.Char                      ( isDigit )
import           Data.List                      ( maximumBy
                                                , sort
                                                )
import           Data.Ord                       ( comparing )

type Coord = (Int, Int, Int)

data Bot = Bot
  { pos :: Coord
  , r :: Int
  } deriving (Show)

ex1 :: [String]
ex1 =
  [ "pos=<0,0,0>, r=4"
  , "pos=<1,0,0>, r=1"
  , "pos=<4,0,0>, r=3"
  , "pos=<0,2,0>, r=1"
  , "pos=<0,5,0>, r=3"
  , "pos=<0,0,3>, r=1"
  , "pos=<1,1,1>, r=1"
  , "pos=<1,1,2>, r=1"
  , "pos=<1,3,1>, r=1"
  ]

parse :: [String] -> [Bot]
parse = map parseLine

parseLine :: String -> Bot
parseLine l = Bot {pos = (x, y, z), r = radius}
 where
  (x, y, z, radius) = read ('(' : cleanUp l ++ ")") :: (Int, Int, Int, Int)
  cleanUp :: String -> String
  cleanUp =
    map (\c -> if isDigit c || c == '-' || c == ',' then c else ' ') . drop 5

manhattan :: Coord -> Coord -> Int
manhattan (a, b, c) (d, e, f) = abs (a - d) + abs (b - e) + abs (c - f)

solve1 :: [Bot] -> Int
solve1 bots = length $ filter isInRange bots
 where
  sb        = strongestBot bots
  range     = r sb
  isInRange = (<= range) . manhattan (pos sb) . pos

strongestBot :: [Bot] -> Bot
strongestBot = maximumBy (comparing r)

main :: IO ()
main = do
  putStrLn "Example: "
  print $ parse ex1
  let pex1 = parse ex1
  putStrLn "Strongest example bot:"
  print $ strongestBot (parse ex1)
  putStrLn "Bots in range of strongst bot:"
  print . solve1 $ pex1
  -- Part 1
  input <- readFile "input.txt"
  let linput = lines input
  let pinput = parse linput
  putStrLn "Part 1:"
  putStrLn "Bots in range of strongst bot:"
  print $ solve1 pinput
