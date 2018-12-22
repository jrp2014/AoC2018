{-# LANGUAGE MultiWayIf #-}

module Main where

import qualified Data.Map as M
import qualified Data.Set as S

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
                     [(x, y) | x <- [0 .. fst limit], y <- [0 .. snd limit]]) $ \p@(x, y) ->
        if | p == target -> 0
           | p == (0, 0) -> 0
           | y == 0 -> x * 16807
           | x == 0 -> y * 48271
           | otherwise ->
             fromIntegral (eriosionLevels M.! (x - 1, y)) *
             fromIntegral (eriosionLevels M.! (x, y - 1))
    eriosionLevels = (`mod` 20183) . fromIntegral . (+ depth) <$> geoIndexes

regionTypes :: Int -> Region -> Region -> M.Map Region RegionType
regionTypes depth limit =
  fmap (toEnum . (`mod` 3) . fromIntegral) . erosionLevels depth limit

ex1 :: M.Map Region RegionType
ex1 = regionTypes 510 (10, 10) (10, 10)

main :: IO ()
main = do
  print $ ex1 M.! (0, 0)
  print $ ex1 M.! (1, 0)
  print $ ex1 M.! (0, 1)
  print $ ex1 M.! (1, 1)
  print $ ex1 M.! (10, 10)
