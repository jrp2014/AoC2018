{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import           Data.Char                      ( isDigit )
import qualified Data.Set                      as S
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
boundingBoxArea (boundingBox -> ((xMin, yMin), (xMax, yMax))) =
  (xMax - xMin) * (yMax - yMin)

display :: Sky -> String
display points = unlines
  [ [ if (x, y) `S.member` positions then '#' else '.' | x <- [xMin .. xMax] ]
  | y <- [yMin .. yMax]
  ]
 where
  ((xMin, yMin), (xMax, yMax)) = boundingBox positions
  positions                    = S.map position points

newPoint :: Int -> Point -> Point
newPoint t pt@Point {..} = pt { position = (x + vx * t, y + vy * t) }
 where
  (x , y ) = position
  (vx, vy) = velocity

newSky :: Int -> Sky -> Sky
newSky t = S.map (newPoint t)

skySignature :: Sky -> Int -> Int
skySignature sky t = boundingBoxArea $ S.map position (newSky t sky)

skySignatures :: Sky -> [(Int, Int)]
skySignatures sky = [ (t, skySignature sky t) | t <- [0 ..] ]

skySignatures' :: Sky -> [(Int, Int)]
skySignatures' sky =
  [ (t, skySignature sky t) | n <- [0 ..] :: [Int], let t = 2 ^ n  :: Int]

secant :: (Integral t) => t -> (t -> t) -> t -> t -> t
secant epsilon f guess1 guess0 =
  let newGuess =
          guess1 - f guess1 * (guess1 - guess0) `div` (f guess1 - f guess0)
      err = abs (newGuess - guess1)
  in  if err < epsilon then newGuess else secant epsilon f newGuess guess1



solve1 :: Sky -> (Int, String)
solve1 sky = (bestT, display $ newSky bestT sky)
 where
  (t0, b0) = head $ skySignatures sky
  (ts, bs) = head $ dropWhile ((< b0) . snd) $ skySignatures' sky -- first boundingBox bigger than where we started

  -- Could use a better initial guess, such as
  -- ts - bs * (ts - t0) `div` (bs -b0) instead of t0 or ts, but the result is
  -- instantaneous as it is
  bestT    = secant 1 (skySignature sky) t0 ts


main :: IO ()
main = do
  putStrLn $ display (parse ex1)
  putStrLn . display $ newSky 3 (parse ex1)
  --print . take 10 $ skySignatures (parse ex1)
  --print . take 10 $ skySignatures' (parse ex1)
  print $ solve1 (parse ex1)
  -- Part 1
  input <- readFile "input.txt"
  let linput          = lines input
  let (bestT, result) = solve1 (parse linput)
  putStrLn result
  -- Part 2
  print bestT
