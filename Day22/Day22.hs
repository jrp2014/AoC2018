{-# LANGUAGE MultiWayIf #-}

module Main where

import qualified Data.Map                      as M
import qualified Data.Set                      as S

data RegionType
  = Rocky
  | Wet
  | Narrow
  deriving (Show, Eq, Ord, Enum)

type Region = (Int, Int)

-- memoized
erosionLevels :: Int -> Region -> Region -> M.Map Region Int
erosionLevels depth limit target = eriosionLevels
 where
  geoIndexes =
    (`M.fromSet` S.fromList
        [ (x, y) | x <- [0 .. fst limit], y <- [0 .. snd limit] ]
      )
      $ \region@(x, y) ->
          if
            | region == target
            -> 0
            | region == (0, 0)
            -> 0
            | y == 0
            -> x * 16807
            | x == 0
            -> y * 48271
            | otherwise
            -> (eriosionLevels M.! (x - 1, y)) * (eriosionLevels M.! (x, y - 1))
  eriosionLevels = (`mod` 20183) . (+ depth) <$> geoIndexes

regionTypes :: Int -> Region -> Region -> M.Map Region RegionType
regionTypes depth limit = fmap (toEnum . (`mod` 3)) . erosionLevels depth limit

regionType :: Int -> Region -> Region -> RegionType
regionType depth limit region = (regionTypes depth limit region) M.! region

riskLevel :: Int -> Region -> Int
riskLevel depth target = sum . fmap fromEnum $ regionTypes depth target target


ex1 :: M.Map Region RegionType
ex1 = regionTypes 510 (10, 10) (10, 10)

main :: IO ()
main = do
  -- Examples
  print $ ex1 M.! (0, 0)
  print $ ex1 M.! (1, 0)
  print $ ex1 M.! (0, 1)
  print $ ex1 M.! (1, 1)
  print $ ex1 M.! (10, 10)
  print $ riskLevel 510 (10, 10)
  --Part 1
  let depths = 11541
  let target = (14, 778)
  print $ riskLevel depths target

