{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Data.Graph.AStar (aStar)
import qualified Data.HashSet as HS
import Data.Hashable (Hashable)
import qualified Data.Map as M
import Data.Maybe (isJust, maybe)
import qualified Data.Set as S
import GHC.Generics (Generic)

data RegionType
  = Rocky
  | Wet
  | Narrow
  deriving (Show, Eq, Ord, Enum)

type Region = (Int, Int)

type Cave = M.Map Region RegionType

manhattan :: Region -> Region -> Int
manhattan (a, b) (c, d) = abs (a - c) + abs (b - d)

-- memoized
erosionLevels :: Int -> Region -> Region -> M.Map Region Int
erosionLevels depth limit target = erosionLevels'
  where
    geoIndexes =
      (`M.fromSet` S.fromList
                     [(x, y) | x <- [0 .. fst limit], y <- [0 .. snd limit]]) $ \region@(x, y) ->
        if | region == target -> 0
           | region == (0, 0) -> 0
           | y == 0 -> x * 16807
           | x == 0 -> y * 48271
           | otherwise ->
             (erosionLevels' M.! (x - 1, y)) * (erosionLevels' M.! (x, y - 1))
    erosionLevels' = (`mod` 20183) . (+ depth) <$> geoIndexes

regionTypes :: Int -> Region -> Region -> M.Map Region RegionType
regionTypes depth limit = fmap (toEnum . (`mod` 3)) . erosionLevels depth limit

regionType :: Int -> Region -> Region -> RegionType
regionType depth limit region = regionTypes depth limit region M.! region

riskLevel :: Int -> Region -> Int
riskLevel depth target = sum . fmap fromEnum $ regionTypes depth target target

ex1 :: M.Map Region RegionType
ex1 = regionTypes 510 (10, 10) (10, 10)

-- Part 2
data Tool
  = Torch
  | ClimbingGear
  deriving (Show, Eq, Ord, Generic) -- GEneric is for Hashable

instance Hashable Tool -- for aStar

type Equipment = Maybe Tool

usable :: RegionType -> Equipment -> Bool
usable Rocky = isJust
usable Wet = (/= Just Torch)
usable Narrow = (/= Just ClimbingGear)

type RegionState = (Equipment, Region)

nexts :: Cave -> RegionState -> [RegionState]
nexts cave (equipment, region) =
  filter (uncurry usable') $ regionStates ++ regionStates'
  where
    usable' :: Equipment -> Region -> Bool
    usable' e = maybe False (`usable` e) . (`M.lookup` cave)
    regionStates :: [RegionState]
    regionStates =
      [ (e, region)
      | e <- [Nothing, Just Torch, Just ClimbingGear]
      , e /= equipment
      ]
    regionStates' :: [RegionState]
    regionStates' = (equipment, ) <$> neighbours region
    neighbours :: Region -> [Region]
    neighbours (x, y) = [(x, y - 1), (x + 1, y), (x, y + 1), (x - 1, y)]

stepsTime :: RegionState -> RegionState -> Int
stepsTime (equipment, region) (equipment', region')
  | equipment == equipment' = manhattan region region'
  | otherwise = manhattan region region' + 7

stepTime :: RegionState -> RegionState -> Int
stepTime (equipment, _) (equipment', _)
  | equipment == equipment' = 1
  | otherwise = 7

path :: Cave -> Region -> Maybe [RegionState]
path cave target =
  (start :) <$>
  aStar (HS.fromList . nexts cave) stepTime (stepsTime finish) (== finish) start
  where
    start = (Just Torch, (0, 0)) :: RegionState
    finish = (Just Torch, target) :: RegionState

pathTime :: [RegionState] -> Int
pathTime = sum . map (uncurry stepTime) . (zip <*> tail)

solve2 :: Int -> Region -> Maybe Int
solve2 depth target = pathTime <$> path cave target
  where
    cave = regionTypes depth (x * 2, y * 2) target -- There may be tighter limits than *2
    (x, y) = target

main :: IO ()
main
  -- Examples
 = do
  print $ ex1 M.! (0, 0)
  print $ ex1 M.! (1, 0)
  print $ ex1 M.! (0, 1)
  print $ ex1 M.! (1, 1)
  print $ ex1 M.! (10, 10)
  print $ riskLevel 510 (10, 10)
  --Part 1
  let depths = 11541 :: Int
  let target = (14, 778) :: Region
  print $ riskLevel depths target
  -- Part 2
  print $ solve2 510 (10, 10)
  print $ solve2 depths target
