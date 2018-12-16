module Main where

import Data.Bits ((.&.), (.|.))
import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap, (!))
import Data.List (intersect)

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

findConsistentMatches :: [Sample] -> Candidates
findConsistentMatches samples =
  IntMap.fromListWith intersect (map findMatches samples)

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

solve1 :: String -> Int
solve1 input = length $ filter (>=3) (map length candidates)
   where
     candidates = map (snd . findMatches) (parsePart1 input)

--assignments :: Candidates -> Assignments
main :: IO ()
main = do
  let sample =
        Sample
          (loadRegisters [3, 2, 1, 1])
          (Instruction 9 2 1 2)
          (loadRegisters [3, 2, 2, 1])
  print $ findMatches sample
  print $ findConsistentMatches [sample]
  print $ parseSample "Before: [3, 2, 1, 1]" "9 2 1 2" "After:  [3, 2, 2, 1]"
  input <- readFile "input1.txt"
  putStrLn "Part 1"
  print $ solve1 input
