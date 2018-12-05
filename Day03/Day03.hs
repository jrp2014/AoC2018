{-# LANGUAGE TupleSections #-}

module Main where

import Data.Char (isDigit)
import Data.List (concatMap)
import qualified Data.Map as Map

type Coord = (Int, Int)

type Fabric = Map.Map Coord Int

type Id = Int

data Claim = Claim
  { identifier :: Id
  , left :: Int
  , top :: Int
  , width :: Int
  , height :: Int
  , bottom :: Int
  , right :: Int
  } deriving (Show)

-- no need for fancy parsing; just strip out the special characters and use read
parse :: String -> Claim
parse s =
  Claim
    { identifier = i
    , left = l
    , top = t
    , width = w
    , height = h
    , bottom = t + h
    , right = l + w
    }
  where
    [i, l, t, w, h] =
      map
        read
        (words
           (map
              (\c ->
                 if isDigit c
                   then c
                   else ' ')
              s))

data Point = Point
  { coord :: Coord
  , ident :: Id
  }

-- Generate the list of coordiates covered by a claim
claimCoords :: Claim -> [Point]
claimCoords Claim { identifier = i
                  , left = l
                  , top = t
                  , width = w
                  , height = h
                  , bottom = b
                  , right = r
                  } =
  [Point {coord = (x, y), ident = i} | x <- [l .. r - 1], y <- [t .. b - 1]]

freqMap :: Ord a => [a] -> Map.Map a Int
freqMap = Map.fromListWith (+) . map (, 1)

cover :: [Claim] -> Fabric
cover = freqMap . map coord . concatMap claimCoords

solve1 :: [Claim] -> Int
solve1 claims = length . filter (>= 2) . Map.elems $ cover claims

ex1 :: [String]
ex1 = ["#1 @ 1,3: 4x4", "#2 @ 3,1: 4x4", "#3 @ 5,5: 2x2"]

findNonOverlappingClaim :: Fabric -> [Claim] -> Int
findNonOverlappingClaim fabric claims =
  head
    [ identifier claim
    | claim <- claims
    , all (== 1) (Map.intersection fabric (cover [claim]))
    ]

solve2 :: [Claim] -> Int
solve2 claims = findNonOverlappingClaim (cover claims) claims

main :: IO ()
main = do
  claims <- readFile "input.txt"
  let lclaims = lines claims
  -- Part One
  print $ solve1 $ map parse ex1
  print $ solve1 $ map parse lclaims
  -- Part Two
  print $ solve2 $ map parse ex1
  print $ solve2 $ map parse lclaims
