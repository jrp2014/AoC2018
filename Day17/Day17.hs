{-# LANGUAGE ViewPatterns #-}

module Main where

import Data.Char (isDigit)
import qualified Data.Map.Strict as M
import Data.Semigroup
import qualified Data.Set as S

type Input = [String]

type Coord = (Int, Int)

ex1 :: Input
ex1 =
  [ "x=495, y=2..7"
  , "y=7, x=495..501"
  , "x=501, y=3..7"
  , "x=498, y=2..4"
  , "x=506, y=1..2"
  , "x=498, y=10..13"
  , "x=504, y=10..13"
  , "y=13, x=498..504"
  ]

parse :: [String] -> S.Set Coord
parse = foldMap parseLine  

parseLine :: String -> S.Set Coord
parseLine ('x':(map read . words . clean -> x:(y0:y1:_))) =
  S.fromList [(x, y) | y <- [y0 .. y1]]
parseLine ('y':(map read . words . clean -> y:(x0:x1:_))) =
  S.fromList [(x, y) | x <- [x0 .. x1]]
parseLine _ = S.empty

clean :: String -> String
clean =
  map
    (\c ->
       if isDigit c
         then c
         else ' ')

-- this will fail if cs is empty
boundingBox :: Foldable f => f Coord -> (Coord, Coord)
boundingBox cs = ((xMin, yMin), (xMax, yMax))
  where
    (Min xMin, Min yMin, Max xMax, Max yMax) =
      flip foldMap cs $ \(x, y) -> (Min x, Min y, Max x, Max y)

display :: Char -> M.Map Coord Char -> String
display d cs =
  unlines
    [ [M.findWithDefault d (x, y) cs | x <- [xMin .. xMax]]
    | y <- [yMin .. yMax]
    ]
  where
    ((xMin, yMin), (xMax, yMax)) = boundingBox (M.keys cs)


showGrid :: S.Set Coord -> String
showGrid cs = display '.' grid
  where
    grid = M.fromSet (const '#') cs

main :: IO ()
main = do
  putStrLn . showGrid $ parse ex1
