{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE BlockArguments #-}
module Day12 (
  doDay12
  ) where

import Data.Semigroup (Endo(..), appEndo, stimes)
import Control.Applicative ((<|>))
import Data.Foldable (foldl')
import Text.Read (readMaybe)

import Lib (addVec)

data Cardinal = N | S | E | W
  deriving (Show, Eq)

-- we normalize turns to a composition of n ccw turns
data Turn = Turn Int
  deriving (Show, Eq)

data Move = Absolute Cardinal Int | Relative Int
  deriving (Show, Eq)

data ShipState = ShipState Cardinal (Int, Int)
  deriving (Show, Eq)

data Instruction = InstructTurn Turn | InstructMove Move
  deriving (Show, Eq)

-- we assume turns are only multiples of 90
toTurn :: String -> Maybe Turn
toTurn ('L':n) = Turn . (`mod` 4) . (`div` 90) <$> readMaybe n
-- a right turn is 3 left turns
toTurn ('R':n) = Turn . (`mod` 4) . (*3) . (`div` 90) <$> readMaybe n
toTurn _ = Nothing

toMove :: String -> Maybe Move
toMove = \case
  'N':n -> Absolute N <$> readMaybe n
  'S':n -> Absolute S <$> readMaybe n
  'E':n -> Absolute E <$> readMaybe n
  'W':n -> Absolute W <$> readMaybe n
  'F':n -> Relative   <$> readMaybe n

parseInstruction :: String -> Maybe Instruction
parseInstruction s = InstructTurn <$> toTurn s <|> InstructMove <$> toMove s

parseInstructions :: String -> Maybe [Instruction]
parseInstructions = traverse parseInstruction . lines

turnCCW :: Cardinal -> Cardinal
turnCCW = \case
  N -> W
  W -> S
  S -> E
  E -> N

turnShip :: Turn -> ShipState -> ShipState
turnShip (Turn n) (ShipState heading pos) = ShipState heading' pos
  where heading' = appEndo (stimes n $ Endo turnCCW) heading

scale n (a,b) = (n*a,n*b)

unit :: Cardinal -> (Int, Int)
unit = \case
  N -> ( 0, 1)
  S -> ( 0,-1)
  E -> ( 1, 0)
  W -> (-1, 0)

move :: ShipState -> Move -> ShipState
move (ShipState heading pos) = \case
  Absolute cardinal n -> ShipState heading (addVec pos (scale n $ unit cardinal))
  Relative          n -> ShipState heading (addVec pos (scale n $ unit heading))

applyInstruction :: ShipState -> Instruction -> ShipState
applyInstruction ss (InstructTurn t) = turnShip t ss
applyInstruction ss (InstructMove m) = move ss m

manhattanFrom0 :: (Int, Int) -> Int
manhattanFrom0 (x,y) = abs x + abs y

part1 :: [Instruction] -> Int
part1 instructions = manhattanFrom0 finalPos
  where initialState = ShipState E (0,0)
        ShipState _ finalPos = foldl' applyInstruction initialState instructions


doDay12 :: IO ()
doDay12 = do
  let fp = "inputs/day12.txt"
  input <- readFile fp
  let Just instructions = parseInstructions input
  print $ part1 instructions

