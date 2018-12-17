module Main where

-- TODO: works,but types are a bit untidy
import Control.Monad (foldM)
import Data.Bits ((.&.), (.|.))
import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap, (!))
import Data.List (foldl')
import Data.Maybe (fromJust)
import qualified Data.Set as S

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

data OpCode
  = Oaddr
  | Oaddi
  | Omulr
  | Omuli
  | Obanr
  | Obani
  | Oborr
  | Obori
  | Osetr
  | Oseti
  | Ogtir
  | Ogtri
  | Ogtrr
  | Oeqir
  | Oeqri
  | Oeqrr
  deriving (Show, Eq, Ord, Enum, Bounded)

type OpCodes = S.Set OpCode

type Microcode = Int -> Int -> Int -> Registers -> Registers

type Candidates = IntMap OpCodes

type Assignments = IntMap Microcode

opCodeMap :: [(OpCode, Microcode)]
opCodeMap
  = [ (Oaddr, \a b c regs -> IntMap.insert c (regs ! a + regs ! b) regs)
    , (Oaddi, \a b c regs -> IntMap.insert c (regs ! a + b) regs)
    , (Omulr, \a b c regs -> IntMap.insert c (regs ! a * regs ! b) regs)
    , (Omuli, \a b c regs -> IntMap.insert c (regs ! a * b) regs)
    , (Obanr, \a b c regs -> IntMap.insert c (regs ! a .&. regs ! b) regs)
    , (Obani, \a b c regs -> IntMap.insert c (regs ! a .&. b) regs)
    , (Oborr, \a b c regs -> IntMap.insert c (regs ! a .|. regs ! b) regs)
    , (Obori, \a b c regs -> IntMap.insert c (regs ! a .|. b) regs)
    , (Osetr, \a _ c regs -> IntMap.insert c (regs ! a) regs)
    , (Oseti, \a _ c regs -> IntMap.insert c a regs)
    , ( Ogtir
      , \a b c regs -> IntMap.insert c (if a > regs ! b then 1 else 0) regs
      )
    , ( Ogtri
      , \a b c regs -> IntMap.insert c (if regs ! a > b then 1 else 0) regs
      )
    , ( Ogtrr
      , \a b c regs ->
        IntMap.insert c (if regs ! a > regs ! b then 1 else 0) regs
      )
    , ( Oeqir
      , \a b c regs -> IntMap.insert c (if a == regs ! b then 1 else 0) regs
      )
    , ( Oeqri
      , \a b c regs -> IntMap.insert c (if regs ! a == b then 1 else 0) regs
      )
    , ( Oeqrr
      , \a b c regs ->
        IntMap.insert c (if regs ! a == regs ! b then 1 else 0) regs
      )
    ]

findMatches :: Sample -> (Int, OpCodes)
findMatches s =
  ( opCode instr
  , S.fromList
      [ name
      | (name, f) <- opCodeMap
      , after s == f (inputA instr) (inputB instr) (outputC instr) (before s)
      ])
  where
    instr = instruction s

findAllMatches :: [Sample] -> Candidates
findAllMatches samples =
  IntMap.fromListWith S.intersection (map findMatches samples)

findConsistentMatches :: Candidates -> IntMap Microcode
findConsistentMatches candidates =
  fmap (\name -> fromJust (lookup name opCodeMap)) nameMap
    -- nameMap :: IntMap Candidates -> IntMap (String, Microcode)
  where
    [nameMap] = foldM pick IntMap.empty (IntMap.toList candidates)

findConsistentMatches' :: Candidates -> IntMap OpCode
findConsistentMatches' candidates = nameMap
    -- nameMap :: IntMap Candidates -> IntMap (String, Microcode)
  where
    [nameMap] = foldM pick IntMap.empty (IntMap.toList candidates)

--pick :: Candidates -> (Int, [[String]]) -> [Candidates]
pick :: IntMap OpCode -> (Int, OpCodes) -> [IntMap OpCode]
pick assigned (op, unassigned) =
  [ IntMap.insert op picked assigned
  | picked <- S.toList unassigned
  , picked `notElem` assigned
  ]

parsePart1 :: String -> [Sample]
parsePart1 = parseBlocks . lines 
  where
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
    [op, a, b, c] = map read (words insts)

parsePart2 :: String -> [Instruction]
parsePart2 s = map (toInstruction . map read . words) $ lines s
  where
    toInstruction [op, a, b, c] = Instruction op a b c

solve1 :: String -> Int
solve1 input = length . filter (>= 3) $ map length candidates
  where
    candidates = map (snd . findMatches) (parsePart1 input)

execute :: IntMap Microcode -> Registers -> Instruction -> Registers
execute microcode registers (Instruction op a b c) =
  (microcode IntMap.! op) a b c registers

solve2 :: String -> String -> Int
solve2 observations program = finalRegisters ! 0
  where
    instructions = parsePart2 program
    microcode = findConsistentMatches . findAllMatches $ parsePart1 observations
    finalRegisters =
      foldl' (execute microcode) (loadRegisters [0, 0, 0, 0]) instructions

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
  samples <- readFile "input1.txt"
  putStrLn "Part 1"
  print $ solve1 samples
  print $ findAllMatches (parsePart1 samples)
  print $ findConsistentMatches' $ findAllMatches $ parsePart1 samples
  instructions <- readFile "input2.txt"
  print $ solve2 samples instructions
