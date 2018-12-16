{-# language GeneralizedNewtypeDeriving #-}
{-# language TypeFamilies #-}
module Main where

import Data.Functor.Compose (Compose(..))
import qualified Data.Vector as V
import Data.Bool (bool)
import Data.Distributive (Distributive(..))
import Data.Functor.Rep (Representable(..), distributeRep)
import Data.Functor.Identity (Identity(..))
import Control.Arrow ((***))
import Control.Comonad.Representable.Store (Store(..), StoreT(..), store, experiment)
import Control.Comonad (Comonad(..))



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
