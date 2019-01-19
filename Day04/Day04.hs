{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Main where

import           Control.Applicative            ( many )
import           Data.Char                      ( isAlphaNum )
import           Data.Foldable                  ( toList )
import           Data.List                      ( maximumBy )
import           Data.List.Split                ( splitOn )
import qualified Data.Map                      as M
import           Data.Ord                       ( comparing )
import           Data.Sort                      ( sortOn )
import           Data.Function                  ( on )
import           Data.Time                      ( LocalTime(..)
                                                , TimeOfDay(..)
                                                , defaultTimeLocale
                                                , parseTimeOrError
                                                )
import qualified Text.Parsec                   as P

data Record =
  Record
    { timeStamp :: LocalTime
    , event :: Event
    }
  deriving (Show)

type Log = [Record]

newtype Guard =
  Guard
    { guard :: Int
    }
  deriving (Eq, Ord, Show)

data Event
  = BeginsShift Guard
  | WakesUp
  | FallsAsleep
  deriving (Eq, Show)

type GuardsEvents = M.Map Guard Record

type TimeCard = M.Map Minute Int -- minute -> nap of times asleep at it

type GuardTimeCard = M.Map Guard TimeCard

--
--
parse :: [String] -> Log
parse = sortOn timeStamp . map parseLine

parseLine :: String -> Record
parseLine input = case splitOn "]" input of
  [ts, ev] -> Record (parseTime $ tail ts) (parseEvent $ cleanInput ev)
  _        -> error $ "failed to parse record: " ++ input
 where
  parseTime  = parseTimeOrError False defaultTimeLocale "%Y-%-m-%-d %H:%M"
  parseEvent = \case
    "falls" : "asleep" : _ -> FallsAsleep
    "wakes" : "up"     : _ -> WakesUp
    "Guard" : g        : _ -> BeginsShift $ Guard (read g)
    _                      -> error "failed to parse event"
  cleanInput = words . map (\c -> if isAlphaNum c then c else ' ')

-- Use parsec to parse the parsed log
--
type Parser = P.Parsec Log ()

type Minute = Int

type Minutes = [Minute]

asMinutes :: LocalTime -> Int
asMinutes = todMin . localTimeOfDay

freqs :: (Foldable f, Ord a) => f a -> M.Map a Int
freqs = M.fromListWith (+) . map (, 1) . toList

-- | Get the key-value pair corresponding to the maximum value in the map
maximumVal :: Ord b => M.Map a b -> Maybe (a, b)
maximumVal = maximumValBy compare

-- | Get the key-value pair corresponding to the maximum value in the map,
-- with a custom comparing function.
--
-- > 'maximumVal' == 'maximumValBy' 'compare'
maximumValBy :: (b -> b -> Ordering) -> M.Map a b -> Maybe (a, b)
maximumValBy c = fmap (maximumBy (c `on` snd))


-- Returns the minutes between a sleep and a wake
nap :: Parser Minutes
nap = do
  Record t0 FallsAsleep <- P.anyToken
  Record t1 WakesUp     <- P.anyToken
  pure [asMinutes t0 .. asMinutes t1 - 1]

guardShift :: Parser (Guard, Minutes)
guardShift = do
  Record _ (BeginsShift grd) <- P.anyToken
  napMinutes                 <- concat <$> many (P.try nap)
  pure (grd, napMinutes)

parseLog :: Parser GuardTimeCard
parseLog = fmap freqs . M.fromListWith (++) <$> many guardShift

buildTimeCards :: Log -> GuardTimeCard
buildTimeCards lg = case P.parse parseLog "" lg of
  Left  x   -> error $ "failed to parse event log" ++ show x
  Right gtc -> gtc


sleepiestGuard :: GuardTimeCard -> (Guard, Int)
sleepiestGuard guards = maximumValBy (comparing sum) guards

main :: IO ()
main = do
  records <- readFile "input.txt"
  let lrecords  = lines records
  let precords  = parse lrecords
  let timeCards = buildTimeCards precords
  print timeCards
  -- Part 1
  --print $  sleepiestGuard timeCards
