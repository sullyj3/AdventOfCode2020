{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE BlockArguments #-}
module Day12 where

import Data.Foldable (foldl')
import Text.Read (readMaybe)

import Lib
import Cardinal


data ShipState = ShipState Cardinal (Int, Int)
  deriving (Show, Eq)

-- waypoint position is relative to ship
data ShipState2 = ShipState2 { position :: (Int, Int)
                             , wayPoint :: (Int, Int) }
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


applyDirection :: ShipState -> Direction -> ShipState
applyDirection (ShipState heading pos) = \case
  Absolute cardinal n -> ShipState heading (pos `addVec` (scale n $ unit cardinal))
  Forward           n -> ShipState heading (pos `addVec` (scale n $ unit heading))
  Turn              n ->
    let heading' = applyN n turn90CCW heading in
        ShipState heading' pos


manhattanFrom0 :: (Int, Int) -> Int
manhattanFrom0 (x,y) = abs x + abs y

part1 :: [Direction] -> Int
part1 instructions = manhattanFrom0 finalPos
  where initialState = ShipState E (0,0)
        ShipState _ finalPos = foldl' applyDirection initialState instructions

--------------------------
-------- Part 2 ----------
--------------------------
initialState2 :: ShipState2
initialState2 = ShipState2 (0,0) (10,1)

-- Multiply by the rotation matrix:
-- (0,-1)
-- (1, 0)
rotate90CCwAround0 :: (Int, Int) -> (Int, Int)
rotate90CCwAround0 (x,y) = (-y, x)

applyDirection2 :: ShipState2 -> Direction -> ShipState2
applyDirection2 ss d = ss


doDay12 :: IO ()
doDay12 = do
  let fp = "inputs/day12.txt"
  input <- readFile fp
  let Just instructions = parseDirections input
  print $ part1 instructions

