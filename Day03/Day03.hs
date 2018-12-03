{-# LANGUAGE TupleSections #-}

module Main where

import Data.Char (isDigit)
import Data.List (concatMap)
import qualified Data.Map as Map

type Coord = (Int, Int)

data Claim = Claim
  { identifier :: Int
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

-- Generate the list of coordiates covered by a claim
claimCoords :: Claim -> [Coord]
claimCoords Claim { identifier = i
                  , left = l
                  , top = t
                  , width = w
                  , height = h
                  , bottom = b
                  , right = r
                  } = [(x, y) | x <- [l .. r - 1], y <- [t .. b - 1]]

freqMap :: Ord a => [a] -> Map.Map a Int
freqMap = Map.fromListWith (+) . map (, 1)

cover :: [Claim] -> Map.Map Coord Int
cover = freqMap . concatMap claimCoords

solve1 :: [Claim] -> Int
solve1 cs = length . filter (>= 2) . Map.elems $ cover cs

ex1 :: [String]
ex1 = ["#1 @ 1,3: 4x4", "#2 @ 3,1: 4x4", "#3 @ 5,5: 2x2"]

solve2 :: Eq a => [[a]] -> [a]
solve2 = undefined

main :: IO ()
main = do
  claims <- readFile "input.txt"
  let lclaims = lines claims
  -- Part One
  print $ solve1 $ map parse ex1
  print $ solve1 $ map parse lclaims
