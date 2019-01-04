module Main where


import qualified Data.Map                      as M
import           Control.Monad                  ( mfilter )
import           Data.Maybe                     ( mapMaybe )

type Coord = (Int, Int)

data Acre = Open | Trees | Lumberyard deriving (Show, Eq, Ord)

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

transform :: Landscape -> Landscape
transform lands = M.mapWithKey change lands
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

display :: Landscape -> [String]
display = (map displayAcre) . M.toAscList
  where
    displayAcre (c, Open) = "." 
    displayAcre (c, Trees) = "|" 
    displayAcre (c, Lumberyard) = "#" 


main :: IO ()
main = undefined
