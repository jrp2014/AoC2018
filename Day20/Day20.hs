{-# LANGUAGE ViewPatterns #-}

module Main where

import Data.Graph.Inductive (UGr, level, mkUGraph)
import Data.Map.Strict (Map, (!), elems, insertLookupWithKey, singleton, size)
import Data.Maybe (fromMaybe)

-- remove spaces and first and last (^, $)
cleanInput :: String -> String
cleanInput = tail . takeWhile (/= '$')

-- nodes ~ Rooms
-- edges ~ Doors
type Coord = (Int, Int)

type Room = Int

type RoomMap = Map Coord Room

mkGraph :: String -> UGr
mkGraph input = mkUGraph (elems rooms) doors
  where
    (rooms, doors) =
      mkGraph' [] (singleton (0, 0) 0) [] (0, 0) (cleanInput input)

mkGraph' ::
     [Coord]
  -> RoomMap
  -> [(Room, Room)]
  -> Coord
  -> String
  -> (RoomMap, [Coord])
mkGraph' stack rooms doors coord ('(':directions) =
  mkGraph' (coord : stack) rooms doors coord directions
mkGraph' stack@(coord:_) rooms doors _ ('|':directions) =
  mkGraph' stack rooms doors coord directions
mkGraph' (coord:stack) rooms doors _ (')':directions) =
  mkGraph' stack rooms doors coord directions
mkGraph' stack rooms doors coord@(x, y) (direction:directions) =
  mkGraph' stack rooms' ((rooms ! coord, room) : doors) coord' directions
  where
    coord' =
      case direction of
        'N' -> (x, y - 1)
        'E' -> (x + 1, y)
        'S' -> (x, y + 1)
        'W' -> (x - 1, y)
        _ -> error $ "invalid direction: " ++ [direction]
    (fromMaybe (size rooms) -> room, rooms') =
      insertLookupWithKey (\_ _ a -> a) coord' (size rooms) rooms
mkGraph' [] rooms doors _ "" = (rooms, doors)
mkGraph' _ _ _ _ _ = error "mismatched parens"

ex1 :: String
ex1 = "^WNE$"

ex2 :: String
ex2 = "^ENWWW(NEEE|SSE(EE|N))$"

ex3 :: String
ex3 = "^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$"

ex4 :: String
ex4 = "^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$"

ex5 :: String
ex5 = "^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$"

-- solving entails making a graph of the rooms (vertices) and doors (edges)
-- level does a breadth first search of the graph, generating a list of room
-- and the length of the path to them tuples, from which the problem can be solved.
solve1 :: String -> Int
solve1 = maximum . fmap snd . level 0 . mkGraph

solve2 :: String -> Int
solve2 = length . filter (>= 1000) . fmap snd . level 0 . mkGraph

main :: IO ()
main = do
  print $ solve1 ex1
  print $ solve1 ex2
  print $ solve1 ex3
  print $ solve1 ex4
  print $ solve1 ex5
 -- Part 1
  input <- readFile "input.txt"
  print $ solve1 input
-- Part 2
  print $ solve2 input
