module Main where

import           Control.Monad                  ( mfilter )
import qualified Data.Map                      as M
import           Data.Maybe                     ( mapMaybe )

type Coord = (Int, Int)

data Acre
  = Open
  | Trees
  | Lumberyard
  deriving (Show, Eq, Ord)

type Landscape = M.Map Coord Acre

neighbours :: Coord -> [Coord]
neighbours (x, y) =
  [ (x + dx, y + dy)
  | dx <- [-1 .. 1]
  , dy <- if dx == 0 then [-1, 1] else [-1 .. 1]
  ]

neighbourCount :: Acre -> Landscape -> Coord -> Int
neighbourCount acre landscape =
  length . mapMaybe (mfilter (== acre) . (`M.lookup` landscape)) . neighbours

minute :: Landscape -> Landscape
minute lands = M.mapWithKey change lands
 where
  change :: Coord -> Acre -> Acre
  change coord acre = case acre of
    Open | neighbourCount Trees lands coord >= 3 -> Trees
         | otherwise                             -> Open
    Trees | neighbourCount Lumberyard lands coord >= 3 -> Lumberyard
          | otherwise -> Trees
    Lumberyard
      | neighbourCount Lumberyard lands coord
        >= 1
        && neighbourCount Trees lands coord
        >= 1
      -> Lumberyard
      | otherwise
      -> Open

resourceValue :: Landscape -> Int
resourceValue lands = wooded * lumberyards
 where
  wooded      = M.size $ M.filter (== Trees) lands
  lumberyards = M.size $ M.filter (== Lumberyard) lands

ex1 :: [String]
ex1 =
  [ ".#.#...|#."
  , ".....#|##|"
  , ".|..|...#."
  , "..|#.....#"
  , "#.#|||#|#|"
  , "...#.||..."
  , ".|....|..."
  , "||...#|.#|"
  , "|.||||..|."
  , "...#.|..|."
  ]

-- This is a more general parser than is strictly necessary; it doesn't need
-- rwos to be of the same length
parse :: [String] -> Landscape
parse ls = M.fromList $ concatMap
  (\(x, xrow) -> map (\(y, a) -> ((x, y), a)) xrow)
  (zip [1 ..] (map parseLine ls))
 where
  parseLine l = zip [1 ..] (map parseCh l)
  parseCh '.' = Open
  parseCh '|' = Trees
  parseCh '#' = Lumberyard

display :: Landscape -> String
display ls = unlines . display' $ concatMap displayAcre (M.toAscList ls)
 where
  display' :: String -> [String]
  display' []  = []
  display' lss = h : display' t where (h, t) = splitAt width lss
  width = maximum $ map fst (M.keys ls)
  displayAcre :: (Coord, Acre) -> String
  displayAcre (_, Open      ) = "."
  displayAcre (_, Trees     ) = "|"
  displayAcre (_, Lumberyard) = "#"

main :: IO ()
main = do
  putStrLn . display $ parse ex1
  let after10 = iterate minute (parse ex1) !! 10
  print $ resourceValue after10
  -- Part 1
  input <- readFile "input.txt"
  let linput = lines input
  print $ resourceValue $ iterate minute (parse linput) !! 10
