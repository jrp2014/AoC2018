{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import           Data.Char                      ( isDigit )
import qualified Data.Set                      as S
import qualified Data.Map                      as M
import           Data.Semigroup

type Position = (Int, Int)
type Velocity = (Int, Int)

data Point = Point {position :: Position, velocity :: Velocity } deriving (Show, Eq, Ord)

type Sky = S.Set Point

ex1 :: [String]
ex1 =
  [ "position=< 9,  1> velocity=< 0,  2>"
  , "position=< 7,  0> velocity=<-1,  0>"
  , "position=< 3, -2> velocity=<-1,  1>"
  , "position=< 6, 10> velocity=<-2, -1>"
  , "position=< 2, -4> velocity=< 2,  2>"
  , "position=<-6, 10> velocity=< 2, -2>"
  , "position=< 1,  8> velocity=< 1, -1>"
  , "position=< 1,  7> velocity=< 1,  0>"
  , "position=<-3, 11> velocity=< 1, -2>"
  , "position=< 7,  6> velocity=<-1, -1>"
  , "position=<-2,  3> velocity=< 1,  0>"
  , "position=<-4,  3> velocity=< 2,  0>"
  , "position=<10, -3> velocity=<-1,  1>"
  , "position=< 5, 11> velocity=< 1, -2>"
  , "position=< 4,  7> velocity=< 0, -1>"
  , "position=< 8, -2> velocity=< 0,  1>"
  , "position=<15,  0> velocity=<-2,  0>"
  , "position=< 1,  6> velocity=< 1,  0>"
  , "position=< 8,  9> velocity=< 0, -1>"
  , "position=< 3,  3> velocity=<-1,  1>"
  , "position=< 0,  5> velocity=< 0, -1>"
  , "position=<-2,  2> velocity=< 2,  0>"
  , "position=< 5, -2> velocity=< 1,  2>"
  , "position=< 1,  4> velocity=< 2,  1>"
  , "position=<-2,  7> velocity=< 2, -2>"
  , "position=< 3,  6> velocity=<-1, -1>"
  , "position=< 5,  0> velocity=< 1,  0>"
  , "position=<-6,  0> velocity=< 2,  0>"
  , "position=< 5,  9> velocity=< 1, -2>"
  , "position=<14,  7> velocity=<-2,  0>"
  , "position=<-3,  6> velocity=< 2, -1>"
  ]


parse :: [String] -> Sky
parse = S.fromList . map parseLine

parseLine :: String -> Point
parseLine s = Point (x, y) (vx, vy)
  where [x, y, vx, vy] = map read (words $ cleanLine s)

cleanLine :: String -> String
cleanLine = map (\c -> if isDigit c || c == '-' then c else ' ')


-- NE and SW coordinates of boulding box
-- this will fail if cs is empty
boundingBox :: Foldable f => f Position -> (Position, Position)
boundingBox ps = ((xMin, yMin), (xMax, yMax))
 where
  (Min xMin, Min yMin, Max xMax, Max yMax) =
    flip foldMap ps $ \(x, y) -> (Min x, Min y, Max x, Max y)

boundingBoxArea :: Foldable f => f Position -> Int
boundingBoxArea (boundingBox -> ((xMin, yMin), (xMax, yMax))) = (xMax-xMin) * (yMax -yMin)

display :: Sky -> String
display points = unlines
  [ [ if (x, y) `S.member` positions then '#' else '.' | x <- [xMin .. xMax] ]
  | y <- [yMin .. yMax]
  ]
 where
  ((xMin, yMin), (xMax, yMax)) = boundingBox positions
  positions                    = S.map position points

newPoint :: Int -> Point -> Point
newPoint t pt@Point{..} = pt { position = (x + vx * t, y + vy * t) }
  where
    (x, y) = position
    (vx, vy) = velocity

newSky :: Int -> Sky -> Sky
newSky t = S.map (newPoint t)



main :: IO ()
main = do
  putStrLn $ display (parse ex1)
  putStrLn . display $ newSky 3 (parse ex1)
