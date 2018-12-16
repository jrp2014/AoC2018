module Main where

import Data.Bits ((.&.), (.|.))
import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap, (!))
import Data.List (foldl', intersect)
import Control.Monad (foldM)
import Data.Maybe (fromJust)

data Instruction = Instruction
  { opCode, inputA, inputB, outputC :: Int
  } deriving (Show)

data Sample = Sample
  { before :: Registers
  , instruction :: Instruction
  , after :: Registers
  } deriving (Show)

type Registers = IntMap Int

loadRegisters :: [Int] -> Registers
loadRegisters = IntMap.fromList . zip [0, 1, 2, 3]

type Semantics = Int -> Int -> Int -> Registers -> Registers

type Candidates = IntMap [String]

type Assignments = IntMap Semantics

opCodes :: [(String, Semantics)]
opCodes =
  [ ("addr", \a b c regs -> IntMap.insert c (regs ! a + regs ! b) regs)
  , ("addi", \a b c regs -> IntMap.insert c (regs ! a + b) regs)
  , ("mulr", \a b c regs -> IntMap.insert c (regs ! a * regs ! b) regs)
  , ("muli", \a b c regs -> IntMap.insert c (regs ! a * b) regs)
  , ("banr", \a b c regs -> IntMap.insert c (regs ! a .&. regs ! b) regs)
  , ("bani", \a b c regs -> IntMap.insert c (regs ! a .&. b) regs)
  , ("borr", \a b c regs -> IntMap.insert c (regs ! a .|. regs ! b) regs)
  , ("bori", \a b c regs -> IntMap.insert c (regs ! a .|. b) regs)
  , ("setr", \a _ c regs -> IntMap.insert c (regs ! a) regs)
  , ("seti", \a _ c regs -> IntMap.insert c a regs)
  , ( "gtir"
    , \a b c regs ->
        IntMap.insert
          c
          (if a > regs ! b
             then 1
             else 0)
          regs)
  , ( "gtri"
    , \a b c regs ->
        IntMap.insert
          c
          (if regs ! a > b
             then 1
             else 0)
          regs)
  , ( "gtrr"
    , \a b c regs ->
        IntMap.insert
          c
          (if regs ! a > regs ! b
             then 1
             else 0)
          regs)
  , ( "eqir"
    , \a b c regs ->
        IntMap.insert
          c
          (if a == regs ! b
             then 1
             else 0)
          regs)
  , ( "eqri"
    , \a b c regs ->
        IntMap.insert
          c
          (if regs ! a == b
             then 1
             else 0)
          regs)
  , ( "eqrr"
    , \a b c regs ->
        IntMap.insert
          c
          (if regs ! a == regs ! b
             then 1
             else 0)
          regs)
  ]

findMatches :: Sample -> (Int, [String])
findMatches s =
  ( opCode instr
  , [ name
    | (name, f) <- opCodes
    , after s == f (inputA instr) (inputB instr) (outputC instr) (before s)
    ])
  where
    instr = instruction s

findAllMatches :: [Sample] -> Candidates
findAllMatches samples =
  IntMap.fromListWith intersect (map findMatches samples)

findConsistentMatches :: Candidates -> IntMap Semantics
findConsistentMatches candidates = fmap (\name -> fromJust (lookup name opCodes)) nameMap
  where
    -- nameMap :: IntMap Candidates -> IntMap (String, Semantics)
    [nameMap] = foldM pick IntMap.empty (IntMap.toList candidates)

findConsistentMatches' :: Candidates -> IntMap String
findConsistentMatches' candidates = nameMap
  where
    -- nameMap :: IntMap Candidates -> IntMap (String, Semantics)
    [nameMap] = foldM pick IntMap.empty (IntMap.toList candidates)

--pick :: Candidates -> (Int, [[String]]) -> [Candidates]
pick :: IntMap String -> (Int, [String]) -> [IntMap String]
pick assigned (op, unassigned) = [ IntMap.insert op picked assigned | picked <- unassigned, picked `notElem` assigned]


parsePart1 :: String -> [Sample]
parsePart1 input = parseBlocks linput
  where
    linput = lines input
    parseBlocks (as:insts:bs:_:rest) =
      parseSample as insts bs : parseBlocks rest
    parseBlocks [] = []

parseSample :: String -> String -> String -> Sample
parseSample bs insts as =
  Sample
    (loadRegisters (read $ drop 8 bs))
    (Instruction op a b c)
    (loadRegisters (read $ drop 8 as))
  where
    [op, a, b, c] =
      read $
      '[' :
      map
        (\ch ->
           if ch == ' '
             then ','
             else ch)
        insts ++
      "]"

parsePart2 :: String -> [Instruction]
parsePart2 s = map toInstruction rwords
  where
    llines = lines s
    lwords = map words llines
    rwords = map (map read) lwords
    toInstruction [op, a, b, c] = Instruction op a b c

solve1 :: String -> Int
solve1 input = length $ filter (>=3) (map length candidates)
   where
     candidates = map (snd . findMatches) (parsePart1 input)

execute :: IntMap Semantics -> Registers -> Instruction -> Registers
execute semantics registers (Instruction op a b c) = (semantics IntMap.! op) a b c registers

solve2 :: String -> String -> Int
solve2 observations program  = finalRegisters ! 0
  where
    instructions = parsePart2 program
    semantics = findConsistentMatches . findAllMatches . findMatches  $ parsePart1 observations
    finalRegisters = foldl' execute (loadRegisters [0,0,0,0]) instructions

--assignments :: Candidates -> Assignments
main :: IO ()
main = do
  let sample =
        Sample
          (loadRegisters [3, 2, 1, 1])
          (Instruction 9 2 1 2)
          (loadRegisters [3, 2, 2, 1])
  print $ findMatches sample
  print $ findAllMatches [sample]
  print $ parseSample "Before: [3, 2, 1, 1]" "9 2 1 2" "After:  [3, 2, 2, 1]"
  input <- readFile "input1.txt"
  putStrLn "Part 1"
  print $ solve1 input
  print $ findAllMatches (parsePart1 input)
  print $ findConsistentMatches' $ findAllMatches $ parsePart1 input
