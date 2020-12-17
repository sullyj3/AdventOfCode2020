{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE BlockArguments #-}
module Day12 (
  doDay12
  ) where

import Data.Semigroup (Endo(..), appEndo, stimes)
import Data.Foldable (foldl')
import Text.Read (readMaybe)

import Lib (addVec)

data Cardinal = N | S | E | W
  deriving (Show, Eq)

data ShipState = ShipState Cardinal (Int, Int)
  deriving (Show, Eq)

data Direction = Absolute Cardinal Int
               | Forward Int
               | Turn Int -- we normalize turns to a composition of n ccw turns by 90
  deriving (Show, Eq)


-- parse a direction string.
-- we assume turns are only multiples of 90
-- a right turn is 3 left turns
parseDirection :: String -> Maybe Direction
parseDirection = \case
  'N':n -> Absolute N <$> readMaybe n
  'S':n -> Absolute S <$> readMaybe n
  'E':n -> Absolute E <$> readMaybe n
  'W':n -> Absolute W <$> readMaybe n
  'F':n -> Forward    <$> readMaybe n
  'L':n -> Turn . (`mod` 4) .        (`div` 90) <$> readMaybe n
  'R':n -> Turn . (`mod` 4) . (*3) . (`div` 90) <$> readMaybe n
  _ -> Nothing

parseDirections :: String -> Maybe [Direction]
parseDirections = traverse parseDirection . lines

turnCCW :: Cardinal -> Cardinal
turnCCW = \case
  N -> W
  W -> S
  S -> E
  E -> N

scale n (a,b) = (n*a,n*b)

unit :: Cardinal -> (Int, Int)
unit = \case
  N -> ( 0, 1)
  S -> ( 0,-1)
  E -> ( 1, 0)
  W -> (-1, 0)

applyDirection :: ShipState -> Direction -> ShipState
applyDirection (ShipState heading pos) = \case
  Absolute cardinal n -> ShipState heading (pos `addVec` (scale n $ unit cardinal))
  Forward           n -> ShipState heading (pos `addVec` (scale n $ unit heading))
  Turn              n ->
    let heading' = appEndo (stimes n $ Endo turnCCW) heading in
        ShipState heading' pos


manhattanFrom0 :: (Int, Int) -> Int
manhattanFrom0 (x,y) = abs x + abs y

part1 :: [Direction] -> Int
part1 instructions = manhattanFrom0 finalPos
  where initialState = ShipState E (0,0)
        ShipState _ finalPos = foldl' applyDirection initialState instructions


doDay12 :: IO ()
doDay12 = do
  let fp = "inputs/day12.txt"
  input <- readFile fp
  let Just instructions = parseDirections input
  print $ part1 instructions

