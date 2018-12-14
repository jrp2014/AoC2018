{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Data.Sequence as Seq

type Recipe = Int

type Recipes = Seq.Seq Recipe

type Score = Int

type Scores = Seq.Seq Score

data ScoreBoard = ScoreBoard
  { recipe1, recipe2 :: Recipe
  , scores :: Scores
  }

newScoreBoard :: ScoreBoard -> (Recipes, ScoreBoard)
newScoreBoard ScoreBoard {..} =
  (recipies, ScoreBoard newRecipe1 newRecipe2 newScores)
  where
    newRecipe1 = (recipe1 + score1 + 1) `mod` Seq.length newScores
    newRecipe2 = (recipe2 + score2 + 1) `mod` Seq.length newScores
    score1 = scores `Seq.index` recipe1
    score2 = scores `Seq.index` recipe2
    recipies = newRecipes recipe1 recipe2
    newScores = scores <> recipies

newRecipes :: Recipe -> Recipe -> Recipes
newRecipes r1 r2
  | a == 0 = Seq.singleton b
  | otherwise = Seq.fromList [a, b]
  where
    (a, b) = divMod (r1 + r2) 10

process :: Recipes
process =
  Seq.empty Seq.|> 3 Seq.|> 7 Seq.|>
  process' (ScoreBoard 0 1 (Seq.fromList [3, 7]))
  where
    process' = rs Seq.>< process' sb
    (rs, sb) = newScoreBoard

main :: IO ()
main = do
  undefined
