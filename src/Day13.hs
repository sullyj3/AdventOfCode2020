{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE BlockArguments #-}
module Day13 where

-- delete unneeded stuff
-- import           Data.Maybe (mapMaybe)
-- import           Data.Foldable (foldl', find)
-- import           Data.List (sort)
import           Text.Read (readMaybe)
-- import           Text.Printf         (printf)
-- import qualified Data.Set            as S
-- import           Data.Set            (Set, (\\))
-- import qualified Data.Map as M
-- import           Data.Map (Map, (!?))
import           Data.List.Split (splitOn)
-- import           Control.Arrow ((>>>))

import Lib


type ID = Int

--------------------------
-------- Parsing ---------
--------------------------

-- return the (earliest timestamp, bus ids)
parsePart1 :: [String] -> Maybe (Int, [ID])
parsePart1 = \case
  [l1,l2] -> do earliest <- readMaybe l1
                busIds <- traverse readMaybe . filter (/="x") . splitOn "," $ l2
                Just (earliest, busIds)
  _ -> Nothing

parsePart2 :: [String] -> Maybe Schedule
parsePart2 = \case
  [_,l2] -> traverse parseTimeSlot . splitOn "," $ l2
  _ -> Nothing
  where parseTimeSlot :: String -> Maybe TimeSlot
        parseTimeSlot = \case
          "x" -> Just Unconstrained
          n   -> Bus <$> readMaybe n

--------------------------
-------- Part 1 ----------
--------------------------

-- given a time and list of busses, return the
-- (next departure time, bus id of that departure)
nextBusAfter :: Int -> [ID] -> (Int, ID)
nextBusAfter t = minimumOn fst
               . map (toFst $ firstArrivalAfter t)


firstArrivalAfter :: Int -> ID -> Int
firstArrivalAfter t busId = busId + (busId * (t `div` busId))


part1 :: Int -> [ID] -> Int
part1 t busIds = waitTime * nextBus
  where (nextDeparture, nextBus) = nextBusAfter t busIds
        waitTime = nextDeparture - t

--------------------------
-------- Part 2 ----------
--------------------------

data TimeSlot = Unconstrained | Bus ID
-- maybe use array? not sure
type Schedule = [TimeSlot]

part2 :: Schedule -> Int
part2 = undefined

--------------------------
---------- IO ------------
--------------------------

doDay13 :: IO ()
doDay13 = do
  let testFp = "inputs/day13test.txt"
  let fp     = "inputs/day13.txt"
  input <- lines <$> readFile fp

  let Just (earliest, busIds) = parsePart1 input
  print $ part1 earliest busIds

  let Just schedule = parsePart2 input
  print $ part2 schedule

