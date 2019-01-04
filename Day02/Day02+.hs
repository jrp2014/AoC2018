{-# language DeriveFunctor #-}

{-
 - From https://bartoszmilewski.com/2018/12/20/open-season-on-hylomorphisms/
 - and  https://github.com/BartoszMilewski/AdventOfCode18
 - with added benchmarking wrapping
 -}

module Main where

import           GHC.Exts                       ( groupWith )
import qualified Data.Map.Strict                as M
import           Criterion.Main
import           Data.List                      ( tails
                                                , foldl'
                                                , sortOn
                                                )
import           Data.List.Unique               ( occurrences )

setupEnv :: IO [String]
setupEnv = do
  txt <- readFile "input.txt"
  return $ lines txt


main :: IO ()
main = defaultMain
  [ env setupEnv $ \cs -> bgroup
    "Bartosz Milewski"
    [ bench "checkSum (Part 1):" $ nf checkSum cs
    , bench "Brute force (Part 2):" $ nf findMatch cs
    , bench "Using Trie (Part 2):" $ nf findMatch1 cs
    , bench "Using hylo (Part 2):" $ nf (head . snd . hylo accum fromList) cs
    , bench "Using hylo (Part 2bis)"
      $ nf (length . fst . hylo accum fromList) cs
    ]
  , env setupEnv $ \cs -> bgroup
    "jrp2014"
    [bench "Part 1" $ nf solve1 cs, bench "Part 2" $ nf solve2 cs]
  ]

-- Calculate the checksum

checkSum :: [String] -> Int
checkSum cs =
  let xs             = fmap twoThree cs
      (twos, threes) = foldl'
        (\(x, x') (b, b') -> (x + fromBool b, x' + fromBool b'))
        (0, 0)
        xs
  in  twos * threes

-- Define a multiset

type Counts a = M.Map a Int

add :: Ord a => Counts a -> a -> Counts a
add cs c = M.insertWith (+) c 1 cs

charCounts :: String -> Counts Char
charCounts s = foldl' add M.empty s

twoThree :: String -> (Bool, Bool)
twoThree s = let xs = M.elems (charCounts s) in (2 `elem` xs, 3 `elem` xs)


-- Brute force solution

distance :: (String, String) -> Int
distance = sum . fmap fromBool . uncurry (zipWith (/=))

fromBool :: Num a => Bool -> a
fromBool False = 0
fromBool True  = 1

findMatch :: [String] -> String
findMatch ss =
  let ps      = [ (s1, s2) | s1 <- ss, s2 <- ss ]
      (s, s') = head $ filter ((== 1) . distance) ps
  in  fmap fst $ filter (uncurry (==)) $ zip s s'

-- Using a Trie

data Trie = Trie [(Char, Int, Trie)]
  deriving (Show, Eq)

insertS :: Trie -> String -> Trie
insertS t         "" = t
insertS (Trie bs) s  = Trie (inS bs s)
 where
  inS ((x, n, t) : bs) (c : cs) = if c == x
    then (c, n + 1, insertS t cs) : bs
    else (x, n, t) : inS bs (c : cs)
  inS [] (c : cs) = [(c, 1, insertS (Trie []) cs)]

mkTrie :: [String] -> Trie
mkTrie = foldl' insertS (Trie [])

match1 :: Trie -> [String]
match1 (Trie bs) = go bs
 where
  go :: [(Char, Int, Trie)] -> [String]
  go ((x, n, t) : bs) =
    let a1s = if n > 1 then (x :) <$> match1 t else []
        a2s = foldMap (exact t) bs
        a3s = go bs -- recurse over list
    in  a1s ++ a2s ++ a3s
  go [] = []
  exact t (_, _, t') = matchAll t t'


-- Find all perfect matches between two Tries  
matchAll :: Trie -> Trie -> [String]
matchAll (Trie bs) (Trie bs') = mAll bs bs'
 where
  mAll :: [(Char, Int, Trie)] -> [(Char, Int, Trie)] -> [String]
  mAll [] [] = [""]
  mAll bs bs' =
    let ps = [ (c, t, t') | (c, _, t) <- bs, (c', _', t') <- bs', c == c' ]
    in  foldMap go ps
  go (c, t, t') = fmap (c :) (matchAll t t')


findMatch1 :: [String] -> String
findMatch1 cs = head $ match1 (mkTrie cs)

-- Recursion schemes

type Algebra f x = f x -> x

type Coalgebra f x = x -> f x

newtype Fix f = In { out :: f(Fix f) }

cata :: Functor f => Algebra f a -> Fix f -> a
cata alg = alg . fmap (cata alg) . out

ana :: Functor f => Coalgebra f a -> a -> Fix f
ana coa = In . fmap (ana coa) . coa

hylo :: Functor f => Algebra f a -> Coalgebra f b -> b -> a
hylo alg coa = alg . fmap (hylo alg coa) . coa

---

data TrieF a = TrieF [(Char, a)]
  deriving (Show, Functor)

-- Define the coalgebra
-- Seed is a list of strings

fromList :: Coalgebra TrieF [String]
fromList ss =
  -- are strings empty? (checking one is enough)
              if null (head ss)
  then TrieF []
  else let sss = groupWith head ss in TrieF $ fmap mkBranch sss

mkBranch :: [String] -> (Char, [String])
mkBranch sss = let c = head (head sss) in (c, fmap tail sss)

showAlg :: Algebra TrieF [String]
showAlg (TrieF []) = []
showAlg (TrieF bs) = fmap (\(c, ss) -> c : offset ss) bs
  where offset ss = concatMap (\s -> "-" ++ s ++ ".") ss

showTrie :: Fix TrieF -> String
showTrie = concat . cata showAlg

-- Define the algebra
-- Accumulatir is a pair of lists of strings

accum :: Algebra TrieF ([String], [String])
accum (TrieF []) = ([""], [])
accum (TrieF bs) = -- b :: (Char, ([String], [String]))
  let -- prepend chars to string in both lists
      pss        = unzip $ fmap prep bs
      (ss1, ss2) = both concat pss
      -- find duplicates
      ss         = concatMap (fst . snd) bs
      mset       = foldl' add M.empty ss
      dups       = M.keys $ M.filter (> 1) mset
  in  (ss1, dups ++ ss2)
 where
  prep :: (Char, ([String], [String])) -> ([String], [String])
  prep (c, pss) = both (fmap (c :)) pss

both :: (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)

{-
 - My own (brute force) version
 -}

solve1 :: [String] -> Int
solve1 = sumTuples . map (foldl' incCount (0, 0) . occurrences)

incCount :: (Int, Int) -> (Int, [a]) -> (Int, Int)
incCount (twos, threes) (2, _) = (twos + 1, threes)
incCount (twos, threes) (3, _) = (twos, threes + 1)
incCount x              _      = x

sumTuples :: [(Int, Int)] -> Int
sumTuples xs = sum twos * sum threes where (twos, threes) = unzip xs

ex1 :: [String]
ex1 = ["abcdef", "bababc", "abbcde", "abcccd", "aabcdd", "abcdee", "ababab"]

-- Part 2
--
-- This is a more general solution than is required if we
-- guarantee that exactly one pair of IDs differ by only 1 char
--
-- Given a list of box IDs produce a list of box ID pairs
-- that can be checked for closeness
-- This version avoid duplicates
combinations :: [a] -> [(a, a)]
combinations l = concatMap (zip l) (tails $ tail l)
-- combinations l = [ (diffCount x y, (x, y)) | x <- l, y <- l ]

diffCount :: Eq a => [a] -> [a] -> Int
diffCount [] [] = 0
diffCount (x : xs) (y : ys) | x /= y    = 1 + diffCount xs ys
                            | otherwise = diffCount xs ys

-- capture the number of differences between a pair of IDs
diffs :: Eq a => [[a]] -> [(Int, ([a], [a]))]
diffs l = map (\z@(x, y) -> (diffCount x y, z)) $ combinations l

-- bestMatch ... one with the fewst diffs wins
bestMatch :: Eq a => [[a]] -> [(Int, ([a], [a]))]
bestMatch l = sortOn fst (diffs l)
-- bestMatch l = filter ((==1) . fst) (diffs l)

commonLetters :: Eq a => [a] -> [a] -> [a]
commonLetters (x : xs) (y : ys) | x == y    = x : commonLetters xs ys
                                | otherwise = commonLetters xs ys
commonLetters [] [] = []

solve2 :: Eq a => [[a]] -> [a]
solve2 l = commonLetters x y where (_, (x, y)) = head $ bestMatch l

ex2 :: [String]
ex2 = ["abcde", "fghij", "klmno", "pqrst", "fguij", "axcye", "wvxyz"]

