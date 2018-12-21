{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main where

import Data.Char (isDigit)
import Data.List (foldl')
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, isNothing)
import Data.Semigroup
import qualified Data.Set as S

type Input = [String]

type Coord = (Int, Int)

type Coords = S.Set Coord

type Trail = [Coord]

up :: Coord -> Coord
up (x, y) = (x, y - 1)

down :: Coord -> Coord
down (x, y) = (x, y + 1)

left :: Coord -> Coord
left (x, y) = (x - 1, y)

right :: Coord -> Coord
right (x, y) = (x + 1, y)

data Here
  = Wet
  | Clay
  deriving (Show, Eq)

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

parse :: [String] -> Coords
parse = foldMap parseLine

parseLine :: String -> Coords
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

showGrid :: Coords -> Coords -> Coords -> String
showGrid clay wet flooded = display '.' grid
  where
    grid =
      M.fromSet (const '#') clay <> M.fromSet (const '|') wet <>
      M.fromSet (const '~') flooded

-- go sideways, if possible, until a way down can be found
flowSideways ::
     Coord -> (Coord -> Coord) -> Coords -> Coords -> (Maybe Coord, Trail)
flowSideways origin direction clay wet = flowSideways' origin []
  where
    flowSideways' from path
      | not (below `S.member` clay) && not (below `S.member` wet) =
        (Just from, path)
      | next `S.member` clay = (Nothing, path)
      | otherwise = flowSideways' next (next : path)
      where
        next = direction from
        below = down from

-- go down, if possible, until an obstruction is reached and then report it
flowDown :: Int -> Coord -> Coords -> Coords -> (Maybe (Coord, Here), Trail)
flowDown bottom origin clay wet = flowDown' origin []
  where
    flowDown' from path
      | y' > bottom = (Nothing, path)
      | next `S.member` clay = (Just (from, Clay), path)
      | next `S.member` wet = (Just (from, Wet), path)
      | otherwise = flowDown' next (next : path)
      where
        next = down from
        y' = snd next

spring :: Coord
spring = (500, 0)

springEternal :: Coords -> (Coords, Coords)
springEternal clay = flowFrom' absoluteBottom spring clay S.empty S.empty
  where
    absoluteBottom = maximum $ S.map snd clay

flowFrom' :: Int -> Coord -> Coords -> Coords -> Coords -> (Coords, Coords)
flowFrom' bottom from clay wetTiles flooded =
  let (obstruction, path) = flowDown bottom from clay flooded
      wetTilesDown = wetTiles `S.union` S.fromList path
   in case obstruction of
        Nothing -> (wetTilesDown, flooded) -- fell out the bottom
        Just (coord, obsType) ->
          let (leftWayDown, leftPath) = flowSideways coord left clay flooded
              (rightWayDown, rightPath) = flowSideways coord right clay flooded
              confined = isNothing leftWayDown && isNothing rightWayDown
              wetTilesSideways =
                S.insert coord $
                S.unions
                  [wetTilesDown, S.fromList leftPath, S.fromList rightPath]
              waysDown =
                filter (not . (`S.member` wetTiles)) $
                catMaybes [leftWayDown, rightWayDown]
           in if confined
               {- flood this level and go back up -}
                then flowFrom'
                       bottom
                       (up coord)
                       clay
                       (S.delete coord wetTilesDown)
                       (S.insert coord $
                        S.unions
                          [flooded, S.fromList leftPath, S.fromList rightPath])
               {- recurse the algorithm from the sides that actually have a way down and weren't visited before -}
                else foldl'
                       (\(wet, floodeded) f ->
                          flowFrom' bottom f clay wet floodeded)
                       (wetTilesSideways, flooded)
                       waysDown

solve1 :: [String] -> Int
solve1 input = S.size $ S.filter (\(_, y) -> y >= minY) $ wet `S.union` flooded
  where
    clay = parse input
    (wet, flooded) = springEternal clay
    minY = minimum $ S.map snd clay

solve2 :: [String] -> Int
solve2 input = S.size $ S.filter (\(_, y) -> y >= minY)  flooded
  where
    clay = parse input
    (wet, flooded) = springEternal clay
    minY = minimum $ S.map snd clay

main :: IO ()
main = do
  let pex1 = parse ex1
  putStrLn $ showGrid pex1 S.empty S.empty
  let (wet, flooded) = springEternal pex1
  putStrLn $ showGrid pex1 wet flooded
  print $ solve1 ex1
  -- Part 1
  input <- readFile "input.txt"
  let linput = lines input
  print $ solve1 linput
  --let clay = parse linput
  --let (wet, flooded) = springEternal clay
  --putStrLn $ take 2000 $ showGrid clay wet flooded
  print $ solve2 linput
