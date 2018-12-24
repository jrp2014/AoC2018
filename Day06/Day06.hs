module Main where

import           Data.Char                      ( isDigit )
import           Data.Semigroup                 ( Min(..)
                                                , Max(..)
                                                )
import           Data.Maybe                     ( catMaybes )
import           Data.List                      ( groupBy
                                                , sortBy
                                                , sort
                                                , group
                                                )
import           Data.Function                  ( on )

type Coord = (Int, Int)
type Coords = [Coord]
type Distance = Int
type BoundingBox = (Coord, Coord)

-- NE and SW coordinates of boulding box
-- this will fail if cs is empty
boundingBox :: Foldable f => f Coord -> BoundingBox
boundingBox ps = ((xMin, yMin), (xMax, yMax))
 where
  (Min xMin, Min yMin, Max xMax, Max yMax) =
    flip foldMap ps $ \(x, y) -> (Min x, Min y, Max x, Max y)

display :: Coords -> String
display coords = unlines
  [ [ if (x, y) `elem` coords then '#' else '.' | x <- [xMin .. xMax] ]
  | y <- [yMin .. yMax]
  ]
  where ((xMin, yMin), (xMax, yMax)) = boundingBox coords

parse :: [String] -> Coords
parse = map parseLine

parseLine :: String -> Coord
parseLine s = (x, y) where [x, y] = map read (words $ cleanLine s)

cleanLine :: String -> String
cleanLine = map (\c -> if isDigit c || c == '-' then c else ' ')

--
--
manhattan :: Coord -> Coord -> Int
manhattan (a, b) (c, d) = abs (a - c) + abs (b - d)

-- the closest coordinate to c from cs
closest :: Coord -> Coords -> Maybe Coord
closest c cs = case closests of
  [(coord, _)] : _ -> Just coord -- there is only 1 closest
  _                -> Nothing
 where
  closests = groupBy ((==) `on` snd)
    $ sortBy (compare `on` snd) [ (coord, manhattan c coord) | coord <- cs ]

biggestArea :: Coords -> Int
biggestArea coords =
  maximum . map length . group . sort . removeEdges $ catMaybes
    [ closest (x, y) coords | x <- [xMin .. xMax], y <- [yMin .. yMax] ]
 where
  -- TODO:  Should we remove areas that include an edge rather that removing
  -- rather than just the edge points themselves?
  removeEdges = filter isNotEdge
  isNotEdge (x, y) = x /= xMin && x /= xMax && y /= yMin && y /= yMax
  ((xMin, yMin), (xMax, yMax)) = boundingBox coords

totalDistance :: Coord -> Coords -> Int
totalDistance c = sum . map  (manhattan c) 


main :: IO ()
main = do
  -- Example
  let pex1 = parse ex1
  putStrLn $ display pex1
  print $ biggestArea pex1
  -- Part 1
  input <- readFile "input.txt"
  let linput = lines input
  let pinput = parse linput
  print $ biggestArea pinput


ex1 :: [String]
ex1 = ["1, 1", "1, 6", "8, 3", "3, 4", "5, 5", "8, 9"]
