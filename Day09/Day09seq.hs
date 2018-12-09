module Main where

--import Data.List (foldl', maximum, splitAt)
import Data.Maybe (fromJust)
import qualified Data.Sequence as Seq

data Tape a = Tape
  { leftReversed :: [a]
  , current :: a
  , right :: [a]
  } deriving (Show)

next :: Tape a -> Tape a
next tape@(Tape [] _ []) = tape
next (Tape ls c []) =
  let (x:xs) = reverse ls
   in Tape [] x (xs ++ [c])
next (Tape ls c (r:rs)) = Tape (c : ls) r rs

previous :: Tape a -> Tape a
previous tape@(Tape [] _ []) = tape
previous (Tape [] c rs) =
  let (x:xs) = reverse rs
   in Tape (xs ++ [c]) x []
previous (Tape (l:ls) c rs) = Tape ls l (c : rs)

shiftFocusN :: Int -> Tape a -> Tape a
shiftFocusN 0 tape = tape
shiftFocusN n tape
  | n > 0 = shiftFocusN (n - 1) $ next tape
  | otherwise = shiftFocusN (n + 1) $ previous tape

insertLeft :: a -> Tape a -> Tape a
insertLeft x (Tape ls c rs) = Tape ls x (c : rs)

deleteRight :: Tape a -> Maybe (Tape a)
deleteRight (Tape [] _ []) = Nothing
deleteRight (Tape ls _ (r:rs)) = Just $ Tape ls r rs
deleteRight (Tape (l:ls) _ []) = Just $ Tape ls l []

-- This is v slow
changeNthElement :: Int -> (a -> a) -> [a] -> [a]
changeNthElement idx transform list
  | idx < 1 = error "index less than 1"
  | otherwise =
    case splitAt (idx - 1) list of
      (front, element:back) -> front ++ transform element : back
      _ -> error $ "list doesn't have an element at index " ++ show idx

type Marble = Int

type Circle = Tape Int

type Score = Int

type Results = Seq.Seq Score

turn :: Marble -> Circle -> (Score, Circle)
turn marble circle
  | marble `mod` 23 == 0 =
    (existingMarble + marble, fromJust (deleteRight shiftedCircle))
  | otherwise = (0, (insertLeft marble . shiftFocusN 2) circle)
  where
    shiftedCircle = shiftFocusN (-7) circle
    existingMarble = current shiftedCircle

game :: Int -> Int -> (Results, Circle)
game nPlayers nMarbles = foldl newScores (initialScores, initialCircle) turns
  where
    newScores (scores, circle) (player, marble) =
      ( if score == 0
          then scores
          else Seq.adjust (+ score) player scores
      , circle')
      where
        (score, circle') = turn marble circle
    players = cycle [1 .. nPlayers]
    marbles = [1 .. nMarbles]
    turns = zip players marbles
    initialScores = Seq.replicate nPlayers 0
    initialCircle = Tape [] 0 []

solve1 :: Int -> Int -> Int
solve1 nPlayers nMarbles = maximum . fst $ game nPlayers nMarbles

main :: IO ()
main
  --- Part 1
 = do
  print $ solve1 9 25
  print $ solve1 10 1618
  print $ solve1 13 7999
  print $ solve1 17 1104
  print $ solve1 21 6111
  print $ solve1 30 5807
  print $ solve1 400 71864
  -- Part 2
  print $ solve1 400 (71864 * 100)
