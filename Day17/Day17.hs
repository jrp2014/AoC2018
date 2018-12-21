{-# LANGUAGE ViewPatterns #-}


-- TODO: There are some neat data structures that do the breadcrumbing that this
-- code does manually.  Use them.

module Main where

import           Data.Char                      ( isDigit )
import           Data.List                      ( foldl' )
import qualified Data.Map.Strict               as M
import           Data.Maybe                     ( catMaybes
                                                , isNothing
                                                )
import           Data.Semigroup                 ( Min(..)
                                                , Max(..)
                                                )
import qualified Data.Set                      as S

type Input = [String]

type Coord = (Int, Int)  -- Could make this a record or a V2, but little point

type Coords = S.Set Coord -- use a set to avoid circularity / step repetition

type Path = [Coord]

up :: Coord -> Coord
up (x, y) = (x, y - 1)

down :: Coord -> Coord
down (x, y) = (x, y + 1)

left :: Coord -> Coord
left (x, y) = (x - 1, y)

right :: Coord -> Coord
right (x, y) = (x + 1, y)


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
parseLine ('x' : (map read . words . clean -> x:(y0:y1:_))) =
  S.fromList [ (x, y) | y <- [y0 .. y1] ]
parseLine ('y' : (map read . words . clean -> y:(x0:x1:_))) =
  S.fromList [ (x, y) | x <- [x0 .. x1] ]
parseLine _ = S.empty

clean :: String -> String
clean = map (\c -> if isDigit c then c else ' ')

-- NE and SW coordinates of boulding box
-- this will fail if cs is empty
boundingBox :: Foldable f => f Coord -> (Coord, Coord)
boundingBox cs = ((xMin, yMin), (xMax, yMax))
 where
  (Min xMin, Min yMin, Max xMax, Max yMax) =
    flip foldMap cs $ \(x, y) -> (Min x, Min y, Max x, Max y)

display :: Char -> M.Map Coord Char -> String
display defaultCh cs = unlines
  [ [ M.findWithDefault defaultCh (x, y) cs | x <- [xMin .. xMax] ]
  | y <- [yMin .. yMax]
  ]
  where ((xMin, yMin), (xMax, yMax)) = boundingBox (M.keys cs)

showScan :: Coords -> Coords -> Coords -> String
showScan clay wet flooded = display '.' grid
 where
  grid =
    M.fromSet (const '#') clay
      <> M.fromSet (const '|') wet
      <> M.fromSet (const '~') flooded

-- go sideways, if possible, until a way down can be found
flowSideways
  :: Coord -> (Coord -> Coord) -> Coords -> Coords -> (Maybe Coord, Path)
flowSideways origin direction clay flooded = flowSideways' origin []
 where
  flowSideways' from path
    | -- Can go down here
      not (below `S.member` clay) && not (below `S.member` flooded)
    = (Just from, path)
    | -- can't flow sideways, as hit clay
      next `S.member` clay = (Nothing, path) -- blocked
    | otherwise = flowSideways' next (next : path) -- keep going sideways
   where
    next  = direction from
    below = down from

-- go down, if possible, until an obstruction is reached and then report it
flowDown :: Int -> Coord -> Coords -> Coords -> (Maybe Coord, Path)
flowDown bottom origin clay flooded = flowDown' origin []
 where
  flowDown' from path
    | y' > bottom = (Nothing, path) -- off grid
    | next `S.member` clay || next `S.member` flooded = (Just from, path)  -- blocked
    | otherwise   = flowDown' next (next : path) -- keep going
   where
    next = down from
    y'   = snd next

source :: Coord
source = (500, 0) -- a fixed point, for this problem

-- given the co-ordinates of clay, find wet and flooded coordinates
flowFrom :: Coords -> (Coords, Coords)
flowFrom clay = flowFrom' source dry dry
 where
  bottom :: Int
  bottom = maximum $ S.map snd clay -- the very bottom
  dry :: Coords
  dry = S.empty -- nothing is wet or floded at the outset

  -- the heart of the matter: produces sets of wet and flooded coordinates
  flowFrom' :: Coord -> Coords -> Coords -> (Coords, Coords)
  flowFrom' from wet flooded =
    let
      (obstruction, path) = flowDown bottom from clay flooded
      wetDown :: Coords
      wetDown             = wet `S.union` S.fromList path -- flowing down wets the path
    in
      case obstruction of
        Nothing -> (wetDown, flooded) -- fell through the bottom
        Just coord -> -- hit clay or flood
          let
            (leftWayDown , leftPath ) = flowSideways coord left clay flooded
            (rightWayDown, rightPath) = flowSideways coord right clay flooded
            toTry                     = S.insert coord
              $ S.unions [wetDown, S.fromList leftPath, S.fromList rightPath] -- ways to explore
            waysDown =
              filter (`S.notMember` wet) -- if it's wet, we've been there
                                         $ catMaybes [leftWayDown, rightWayDown]
          in
            if isNothing leftWayDown && isNothing rightWayDown
              -- blocked, so flood this level and go back up
              then flowFrom'
                (up coord)
                (S.delete coord wetDown) -- wet coord will be flooded
                (S.insert coord $ S.unions
                  [flooded, S.fromList leftPath, S.fromList rightPath]
                )
              -- recurse the algorithm from the coords that have a way down and weren't visited before
              else foldl' (\(wt, fldd) frm -> flowFrom' frm wt fldd)
                          (toTry, flooded)
                          waysDown

solve1 :: [String] -> Int
solve1 input = S.size $ S.filter ((>= minY) . snd) $ wet `S.union` flooded
 where
  clay                 = parse input
  (wet      , flooded) = flowFrom clay
  ((_, minY), _      ) = boundingBox clay

solve2 :: [String] -> Int
solve2 input = S.size $ S.filter ((>= minY) . snd) flooded
 where
  clay                 = parse input
  (_        , flooded) = flowFrom clay
  ((_, minY), _      ) = boundingBox clay

main :: IO ()
main = do
  let pex1 = parse ex1
  putStrLn $ showScan pex1 S.empty S.empty
  let (wet, flooded) = flowFrom pex1
  putStrLn $ showScan pex1 wet flooded
  print $ solve1 ex1
  -- Part 1
  input <- readFile "input.txt"
  let linput = lines input
  print $ solve1 linput
  -- Patr 2
  print $ solve2 linput
