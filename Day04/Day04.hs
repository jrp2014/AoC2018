{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Control.Applicative (many)
import Data.Char (isAlphaNum)
import Data.Foldable (toList)
import Data.List (maximumBy)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Ord (comparing)
import Data.Sort (sortOn)
import Data.Time
  ( LocalTime(..)
  , TimeOfDay(..)
  , defaultTimeLocale
  , parseTimeOrError
  )
import qualified Text.Parsec as P

data Record =
  Record
    { timeStamp :: LocalTime
    , event :: Event
    }
  deriving (Show)

type Log = [Record]

newtype Guard =
  Guard
    { guardID :: Int
    }
  deriving (Eq, Ord, Show)

data Event
  = BeginsShift Guard
  | WakesUp
  | FallsAsleep
  deriving (Eq, Show)

type GuardsEvents = M.Map Guard Record

type TimeCard = M.Map Minute Int -- minute -> nap of times asleep at it

type GuardsTimeCard = M.Map Guard TimeCard

--
--
parse :: [String] -> Log
parse = sortOn timeStamp . map parseLine

parseLine :: String -> Record
parseLine input =
  case splitOn "]" input of
    [ts, ev] -> Record (parseTime $ tail ts) (parseEvent $ cleanInput ev)
    _ -> error $ "failed to parse record: " ++ input
  where
    parseTime = parseTimeOrError False defaultTimeLocale "%Y-%-m-%-d %H:%M"
    parseEvent =
      \case
        "falls":"asleep":_ -> FallsAsleep
        "wakes":"up":_ -> WakesUp
        "Guard":g:_ -> BeginsShift $ Guard (read g)
        _ -> error "failed to parse event"
    cleanInput =
      words .
      map
        (\c ->
           if isAlphaNum c
             then c
             else ' ')

-- Use parsec to parse the parsed log
--
type Parser = P.Parsec Log ()

type Minute = Int

type Minutes = [Minute]

asMinutes :: LocalTime -> Int
asMinutes = todMin . localTimeOfDay

freqs :: (Foldable f, Ord a) => f a -> M.Map a Int
freqs = M.fromListWith (+) . map (, 1) . toList

-- Returns the minutes between a sleep and a wake
nap :: Parser Minutes
nap = do
  Record t0 FallsAsleep <- P.anyToken
  Record t1 WakesUp <- P.anyToken
  pure [asMinutes t0 .. asMinutes t1 - 1]

guardShift :: Parser (Guard, Minutes)
guardShift = do
  Record _ (BeginsShift grd) <- P.anyToken
  napMinutes <- concat <$> many (P.try nap)
  pure (grd, napMinutes)

parseLog :: Parser GuardsTimeCard
parseLog = fmap freqs . M.fromListWith (++) <$> many guardShift

buildTimeCards :: Log -> GuardsTimeCard
buildTimeCards lg =
  case P.parse parseLog "" lg of
    Left x -> error $ "failed to parse event log" ++ show x
    Right gtc -> gtc

minutesAsleep :: GuardsTimeCard -> M.Map Guard Int
minutesAsleep = M.map sum

mostMinutesAsleep :: M.Map Guard Int -> (Guard, Int)
mostMinutesAsleep = maximumBy (comparing snd) . M.toList


solve :: ([Int] -> Int) -> GuardsTimeCard -> Int
solve f gs = (fst . maximumBy (comparing snd) . M.toList . snd $ target) * (fst target)
    where target = maximumBy (comparing maxMinutes) . filter (not . null . snd) . M.toList $ gs
          maxMinutes = f . M.elems . snd


solve1 :: GuardsTimeCard -> Int
solve1 gtc = guardID sleepiestGuard * sleepiestMinute
  where
    sleepiestGuard = fst . mostMinutesAsleep $ minutesAsleep gtc
    sleepiestMinute = fst .
      maximumBy (comparing snd) . M.toList $ gtc M.! sleepiestGuard

worstMinute :: TimeCard -> Maybe Int
worstMinute tc
  | M.null tc = Nothing
  | otherwise = Just . fst . maximumBy (comparing snd) $ M.toList tc

solve2 :: GuardsTimeCard -> Int
solve2 gtc = guardID worstGuard * worstMin
  where
    worstMinutes ::  M.Map Guard Int
    worstMinutes = M.mapMaybe worstMinute gtc
    (worstGuard, worstMin)  = maximumBy (comparing snd) $  M.toList worstMinutes

ex1 :: [String]
ex1 = ["[1518-11-01 00:00] Guard #10 begins shift",
  "[1518-11-01 00:05] falls asleep",
  "[1518-11-01 00:25] wakes up",
  "[1518-11-01 00:30] falls asleep",
  "[1518-11-01 00:55] wakes up",
  "[1518-11-01 23:58] Guard #99 begins shift",
  "[1518-11-02 00:40] falls asleep",
  "[1518-11-02 00:50] wakes up",
  "[1518-11-03 00:05] Guard #10 begins shift",
  "[1518-11-03 00:24] falls asleep",
  "[1518-11-03 00:29] wakes up",
  "[1518-11-04 00:02] Guard #99 begins shift",
  "[1518-11-04 00:36] falls asleep",
  "[1518-11-04 00:46] wakes up",
  "[1518-11-05 00:03] Guard #99 begins shift",
  "[1518-11-05 00:45] falls asleep",
  "[1518-11-05 00:55] wakes up~"]

main :: IO ()
main = do
  records <- readFile "input.txt"
  let lrecords = lines records
  let precords = parse lrecords
  let timeCards = buildTimeCards precords
  let pex1 = parse ex1
  let tcex1 = buildTimeCards pex1
  -- Part 1
  print $ solve1 tcex1
  print $ solve1 timeCards
  -- Part 2
  print $ solve2 tcex1
  print $ solve2 timeCards
