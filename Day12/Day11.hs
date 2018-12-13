module Main where

import Data.List (tails)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)

type Plant = Char

type Rules a = Map.Map (a, a, a, a, a) a

newtype State a =
  State [a]
  deriving (Show)

parseState :: String -> State Plant
parseState = State

parseRules :: [String] -> Rules Plant
parseRules s = Map.fromList $ map parseRule s
  where
    parseRule :: String -> ((Plant, Plant, Plant, Plant, Plant), Plant)
    parseRule [a, b, c, d, e, ' ', '=', '>', ' ', p] = ((a, b, c, d, e), p)

applyRules :: Rules Plant -> [Plant] -> Plant
applyRules r [a, b, c, d, e] = fromMaybe '.' (Map.lookup (a, b, c, d, e) r)

parse :: [String] -> (State Plant, Rules Plant)
parse (s0:_:rs) = (ps, prs)
  where
    ps = parseState . drop 15 $ s0
    prs = parseRules rs

windows :: Int -> [a] -> [[a]]
windows m = foldr (zipWith (:)) (repeat []) . take m . tails

padding :: [Plant]
padding = "..."

paddingLength :: Int
paddingLength = length padding

step :: Rules Plant -> State Plant -> State Plant
step r (State plant) = State $ map (applyRules r) $ windows 5 padplant
  where
    padplant = padding ++ plant ++ padding

countPots :: Int -> State Plant -> Int
countPots itern (State s) = sum $ zipWith (curry countPot) [-itern ..] s
  where
    countPot (i, p) =
      if p == '#'
        then i
        else 0

ex1 :: [String]
ex1 =
  [ "initial state: #..#.#..##......###...###"
  , ""
  , "...## => #"
  , "..#.. => #"
  , ".#... => #"
  , ".#.#. => #"
  , ".#.## => #"
  , ".##.. => #"
  , ".#### => #"
  , "#.#.# => #"
  , "#.### => #"
  , "##.#. => #"
  , "##.## => #"
  , "###.. => #"
  , "###.# => #"
  , "####. => #"
  ]

solve1 :: Int -> [String] -> [String]
solve1 n s =
  map (\(c, sps@(State ps)) -> show (c, ps, countPots c sps)) $
  zip [0 .. n] $ iterate (step rs) s0
  where
    (s0, rs) = parse s

solve2 :: Int -> [String] -> [(Int, Int)]
solve2 n s =
  map (\(c, sps) -> (c, countPots c sps)) $ zip [0 .. n] $ iterate (step rs) s0
  where
    (s0, rs) = parse s

diff :: [(Int, Int)] -> [(Int, Int, Int)]
diff s = zipWith (\(a, b) (_, d) -> (a, b, d - b)) s (tail s)

findConstantDiff :: [(Int, Int, Int)] -> (Int, Int, Int)
findConstantDiff ((a, b, c):(d, e, f):(g, h, i):xs)
  | c == f && c == i = (a, b, c)
  | otherwise = findConstantDiff ((d, e, f) : (g, h, i) : xs)

main :: IO ()
main = do
  putStrLn $ last $ solve1 20 ex1
  notes <- readFile "input.txt"
  let lnotes = lines notes
  -- Part One
  putStrLn $ last $ solve1 20 lnotes
  --- Part Two
  print $ diff $ solve2 200 lnotes
  let (repeatsFrom, repeatPotsSum, delta) =
        findConstantDiff (diff $ solve2 200 lnotes)
  print $ repeatPotsSum + (delta * (50000000000 - repeatsFrom))
