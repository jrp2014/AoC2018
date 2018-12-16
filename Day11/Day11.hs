module Main where

import Data.List (maximumBy)
import Data.Map.Strict (Map, (!), empty, fromList, insert)
import Data.Ord (comparing)

type Serial = Int

type Coord = (Int, Int)

type Power = Int

type Grid = Map Coord Power

gridSize :: Int
gridSize = 300

powerAt :: Serial -> Coord -> Power
powerAt serialNumber (x, y) = totalPower
  where
    rackID = x + 10
    initialPower = ((rackID * y) + serialNumber) * rackID
    hundredsDigit = initialPower `div` 100 `mod` 10
    totalPower = hundredsDigit - 5

fuelCellGrid :: Serial -> Int -> Grid
fuelCellGrid serialNumber size =
  fromList
    [((x, y), powerAt serialNumber (x, y)) | x <- [1 .. size], y <- [1 .. size]]

-- Direct appproach
totalPowerAt :: Grid -> Int -> Coord -> (Coord, Power)
totalPowerAt grid squareSize (x, y) = ((x, y), powerSum)
  where
    powerSum =
      sum $
      map
        (grid !)
        [ (x + x', y + y')
        | x' <- [0 .. squareSize - 1]
        , y' <- [0 .. squareSize - 1]
        ]

maxPower :: Int -> Grid -> Int -> (Coord, Power)
maxPower size grid squareSize =
  maximumBy (comparing snd) $
  map
    (totalPowerAt grid squareSize)
    [ (x, y)
    | x <- [1 .. size - (squareSize - 1)]
    , y <- [1 .. size - (squareSize - 1)]
    ]

-- fr testing
maxPower'''' :: Int -> Grid -> Int -> [(Coord, Power)]
maxPower'''' size grid squareSize =
  map
    (totalPowerAt grid squareSize)
    [ (x, y)
    | x <- [1 .. size - (squareSize - 1)]
    , y <- [1 .. size - (squareSize - 1)]
    ]

-- Using summed area values, rather than nominal power, in the Grid
fuelCellGrid' :: Serial -> Int -> Grid
fuelCellGrid' serialNumber size =
  foldl
    powerAt'
    zeroedEdges
    [(col, row) | col <- [1 .. size], row <- [1 .. size]]
  where
    zeroedEdges = foldl zero (insert (0, 0) 0 empty) coords
      where
        coords =
          [(col, 0) | col <- [1 .. size]] ++ [(0, row) | row <- [1 .. size]]
        zero g c = insert c 0 g
    powerAt' grid coord@(x, y) = insert coord powerLevel grid
      where
        powerLevel =
          powerAt serialNumber coord + grid ! (x, y - 1) + grid ! (x - 1, y) -
          grid ! (x - 1, y - 1)

--totalPowerAt' :: Serial -> Int -> Int -> Coord -> (Coord, Power)
totalPowerAt' :: Grid -> Int -> Coord -> (Coord, Power)
totalPowerAt' grid squareSize c@(x0, y0) =
  ( c
  , grid ! (x1', y1') + grid ! (x0', y0') - grid ! (x1', y0') -
    grid ! (x0', y1'))
  where
    x0' = x0 - 1
    y0' = y0 - 1
    x1' = x0' + squareSize
    y1' = y0' + squareSize

maxPower' :: Int -> Grid -> Int -> (Coord, Power)
maxPower' size grid squareSize =
  maximumBy (comparing snd) $
  map
    (totalPowerAt' grid squareSize)
    [ (x, y)
    | x <- [1 .. size - (squareSize - 1)]
    , y <- [1 .. size - (squareSize - 1)]
    ]

-- for tsting
maxPower'' :: Int -> Grid -> Int -> [(Coord, Power)]
maxPower'' size grid squareSize =
  map
    (totalPowerAt' grid squareSize)
    [ (x, y)
    | x <- [1 .. size - (squareSize - 1)]
    , y <- [1 .. size - (squareSize - 1)]
    ]

-- Part 2
--maxPowers' :: Int -> Grid -> )Coord, Power)
-- (((90,269),113),16)
maxPowers' :: Int -> Grid -> (Int, Int, Int)
maxPowers' size grid = (x, y, maxsz)
  where
    (((x, y), _), maxsz) =
      maximumBy
        (comparing (snd . fst))
        [(maxPower' size grid sz, sz) | sz <- [1 .. size]]

main :: IO ()
main
  -- Examples
 = do
  putStrLn "Examples: "
  print $ powerAt 57 (122, 79)
  print $ powerAt 39 (217, 196)
  print $ powerAt 71 (101, 153)
  putStrLn "maxPower 18 intermediates in direct and summed area versions"
  print $ take 20 $ maxPower'''' gridSize (fuelCellGrid 18 gridSize) 3
  print $ take 20 $ maxPower'' gridSize (fuelCellGrid' 18 gridSize) 3
  putStrLn "maxPower 42 in direct and summed area versions"
  print $ maxPower gridSize (fuelCellGrid 42 gridSize) 3
  print $ maxPower' gridSize (fuelCellGrid' 42 gridSize) 3
  putStrLn "totalPowerAt grid .."
  let g = fuelCellGrid 57 gridSize
  print $ totalPowerAt g 3 (122, 79)
  let g' = fuelCellGrid' 57 gridSize
  print $ totalPowerAt' g' 3 (122, 79)
  --  Part 1
  putStrLn "Part 1:"
  print $ maxPower gridSize (fuelCellGrid 7857 gridSize) 3
  print $ maxPower' gridSize (fuelCellGrid' 7857 gridSize) 3
  putStrLn "Part 2:"
  print $ maxPowers' gridSize (fuelCellGrid' 18 gridSize)
  print $ maxPowers' gridSize (fuelCellGrid' 42 gridSize)
  print $ maxPowers' gridSize (fuelCellGrid' 7857 gridSize)
