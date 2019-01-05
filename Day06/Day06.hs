module Main where

import           Data.Char                      ( isDigit )
import           Data.List                      ( group
                                                , sort
                                                , sortOn
                                                )
import           Data.Maybe                     ( catMaybes )
import           Data.Semigroup                 ( Max(..)
                                                , Min(..)
                                                )

type Coord = (Int, Int)

type Coords = [Coord]

type Distance = Int

type Distances = [(Distance, Coord)]

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

distances :: Coord -> Coords -> Distances
distances coord = map (\c -> (manhattan coord c, c))

-- the closest coordinate to c from cs
closest :: Coord -> Coords -> Maybe Coord
closest c cs = coordIfIsUnique . sortOn fst $ distances c cs
 where
  coordIfIsUnique :: Distances -> Maybe Coord
  coordIfIsUnique ((dist1, c1) : (dist2, _) : _) =
    if dist1 == dist2 then Nothing else Just c1
  coordIfIsUnique [(_, c1)] = Just c1
  coordIfIsUnique []        = Nothing

biggestArea :: Coords -> Int
biggestArea coords =
  maximum           -- return the biggest area
    . map length    -- get the size of each area
    . group         -- group areas of the same distance
    . sort          -- sort (by distance (probably superfluous))
    . dropEdges     -- if the closest coord is an edge point, drop this
    . catMaybes     -- drops Nothings (where >1 of coords is closest)
    $ [ closest (x, y) coords | x <- [xMin .. xMax], y <- [yMin .. yMax] ]
 where
  dropEdges = filter isNotEdge
  isNotEdge (x, y) = x /= xMin && x /= xMax && y /= yMin && y /= yMax
  ((xMin, yMin), (xMax, yMax)) = boundingBox coords

totalDistance :: Coord -> Coords -> Distance
totalDistance c = sum . map (manhattan c)

main :: IO ()
main = do
  let pex1 = parse ex1
  putStrLn $ display pex1
  print $ biggestArea pex1
  -- Part 1
  input <- readFile "input.txt"
  let linput = lines input
  let pinput = parse linput
  print $ biggestArea pinput
  -- Part 2
  print $ totalDistance (4, 3) pex1

ex1 :: [String]
ex1 = ["1, 1", "1, 6", "8, 3", "3, 4", "5, 5", "8, 9"]
