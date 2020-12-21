{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE BlockArguments #-}
module Day13 where

-- delete unneeded stuff
import           Data.Maybe (mapMaybe)
-- import           Data.Foldable (find)
-- import           Data.Function (fix)
-- import           Data.List (sort)
import           Text.Read (readMaybe)
-- import           Text.Printf         (printf)
-- import qualified Data.Set            as S
-- import           Data.Set            (Set, (\\))
-- import qualified Data.Map as M
-- import           Data.Map (Map, (!?))
import           Data.List.Split (splitOn)
-- import           Control.Arrow ((>>>))
-- import           Debug.Trace

import Lib
import ChineseRemainder


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

parsePart2 :: [String] -> Maybe [(Integer, Integer)]
parsePart2 = \case
  [_,l2] -> Just $ mapMaybe parseTimeSlot . zip [0..] . splitOn "," $ l2
  _ -> Nothing
  where parseTimeSlot :: (Integer, String) -> Maybe (Integer, Integer)
        parseTimeSlot = \case
          (_,"x") -> Nothing
          (i, n)  -> (,) i <$> readMaybe n

--------------------------
-------- Part 1 ----------
--------------------------

-- given a time and list of busses, return the
-- (next departure time, bus id of that departure)
nextBusAfter :: Int -> [ID] -> (Int, ID)
nextBusAfter t = minimumOn fst
               . map (toFst $ firstArrivalAfter t)


firstArrivalAfter :: Int -> ID -> Int
firstArrivalAfter t busId 
  | t `mod` busId == 0 = t
  | otherwise = busId + (busId * (t `div` busId))


part1 :: Int -> [ID] -> Int
part1 t busIds = waitTime * nextBus
  where (nextDeparture, nextBus) = nextBusAfter t busIds
        waitTime = nextDeparture - t

--------------------------
-------- Part 2 ----------
--------------------------

part2 :: [(Integer, Integer)] -> Integer
part2 input = t0
  where (rems, mods) =
          unzip $ map (\(offset, modulus) -> ((modulus-offset) `mod` modulus, modulus))
                      input
        Right t0 = chineseRemainder rems mods


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

