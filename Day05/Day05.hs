module Main where

import Data.Char (toLower)
import Data.List (minimum)
import Data.List.Extra (nubOrd)

type Polymer = String

type Unit = Char

solve :: Polymer -> Polymer
solve = foldr step []
  where
    reacts :: Unit -> Unit -> Bool
    reacts u v = toLower u == toLower v && u /= v
    step :: Unit -> String -> String
    step u (v:vs)
      | reacts u v = vs
    step u us = u : us

solve1 :: Polymer -> Int
solve1 = length . solve

ex1 :: Polymer
ex1 = "dabAcCaCBAcCcaDA"

removeUnit :: Unit -> Polymer -> Polymer
removeUnit u = filter ((/= u) . toLower)

solveShorter :: Polymer -> [Int]
solveShorter pps = [length . solve $ removeUnit u ps | u <- units]
  where
    ps = solve pps
    units = nubOrd . map toLower $ ps -- not worth doing
    -- units = ['a' .. 'z']

solve2 :: Polymer -> Int
solve2 = minimum . solveShorter

main :: IO ()
main = do
  polymers <- readFile "input.txt"
  let lpolymers = head $ lines polymers
  -- Part One
  print $ solve1 ex1
  print $ solve1 lpolymers
  -- Part Two
  print $ solve2 ex1
  print $ solve2 lpolymers
