{-# LANGUAGE LambdaCase #-}

module Main where

import           Data.Char                      ( isAlphaNum )
import           Data.List.Split                ( splitOn )
import           Data.Time                      ( LocalTime(..)
                                                , TimeOfDay(..)
                                                , defaultTimeLocale
                                                , parseTimeOrError
                                                )

data Record =
  Record
    { timeStamp :: LocalTime
    , event :: Event
    } deriving (Show)

newtype Guard =
  Guard
    { guard :: Int
    }
  deriving (Eq, Show)

data Event
  = BeginsShift Guard
  | WakesUp
  | FallsAsleep
  deriving (Eq, Show)

parse :: [String] -> [Record]
parse = map parseLine

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

main :: IO ()
main = do
  records <- readFile "input.txt"
  let lrecords = lines records
  print $  parse lrecords
