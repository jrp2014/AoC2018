module Main where

import Data.List (nub, partition, sort)
import Graph
import Data.Char (ord)
import qualified Data.Map as M
import qualified Data.Set as S

type Rule = Graph Char

parse :: [String] -> [(Char, Char)]
parse = map (parseLine . words)
  where
    parseLine [_, [p], _, _, _, _, _, [s], _, _] = (p, s)

toRule :: [(Char, Char)] -> Rule
toRule rs = Graph verts rs
  where
    verts = sort $ nub $ map fst rs ++ map snd rs

ex1 :: [String]
ex1 =
  [ "Step C must be finished before step A can begin."
  , "Step C must be finished before step F can begin."
  , "Step A must be finished before step B can begin."
  , "Step A must be finished before step D can begin."
  , "Step B must be finished before step E can begin."
  , "Step D must be finished before step E can begin."
  , "Step F must be finished before step E can begin."
  ]

solve1 :: [(Char, Char)] -> String
solve1 = tsort . toRule

-- Part 2
type Task = Char

-- map from task to its dependencies
type Dependencies = M.Map Task (S.Set Task)

mkDependencies :: [(Char, Char)] -> Dependencies
mkDependencies deps = M.union dependents independents
    -- TODO:: simplify
  where
    dependents =
      M.fromListWith S.union .
      map (\(before, after) -> (after, S.singleton before)) $
      deps
    independents :: Dependencies
    independents = M.fromList . flip zip (repeat S.empty) . map fst $ deps

data Worker = Worker
  { name :: Char
  , remainingTime :: Int
  }

emptyWorker :: Worker
emptyWorker = Worker ' ' 0

solve2 :: Dependencies -> [Worker] -> Int -> Int
solve2 deps workers time
  | M.null deps = time + (maximum . map remainingTime $ workers)
  | otherwise =
    let elapsedTime =
          minOrDefault 0 . filter (/= 0) . map remainingTime $ workers
        (done, working) =
          partition ((== 0) . remainingTime) .
          map
            (\w ->
               Worker
                 (name w)
                 (max 0 . subtract elapsedTime . remainingTime $ w)) $
          workers
        clearedDeps = foldr ((fmap . S.delete) . name) deps done
        (newWorkers, newDeps) =
          foldr assign (working, clearedDeps) [1 .. length done]
     in solve2 newDeps newWorkers (time + elapsedTime)
  where
    minOrDefault :: Int -> [Int] -> Int
    minOrDefault n ns =
      if null ns
        then n
        else minimum ns
    duration :: Task -> Int
    duration task = ord task - ord 'A' + 61 -- or 1 for example
    assign :: Int -> ([Worker], Dependencies) -> ([Worker], Dependencies)
    assign _ (w, d)
      | M.null $ M.filter S.null d = (emptyWorker : w, d)
      | otherwise =
        let task = fst . M.findMax . M.filter S.null $ d
         in (Worker task (duration task) : w, M.delete task d)

main :: IO ()
main = do
  rules <- readFile "input.txt"
  let prules = parse $ lines rules
  putStrLn "Part 1, examples:"
  print $ parse ex1
  print $ solve1 $ parse ex1
  -- Part One
  putStrLn "Part 1:"
  print $ solve1 prules
  -- Part Two
  putStrLn " Part 2:"
  print $ solve2 (mkDependencies prules) (replicate 5 emptyWorker) 0
