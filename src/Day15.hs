-- note: looks like the fastest solutions just use an unboxed array (eg massiv)

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE BlockArguments #-}
module Day15 where

import Data.List.Split (splitWhen)
import           Data.List (unfoldr)
import           Text.Read (readMaybe)
import qualified Data.Map as M
import           Data.Map (Map, (!?))

-------- Parsing ---------
--------------------------

parse :: String -> Maybe [Integer]
parse = traverse readMaybe . splitWhen (==',')

------- Solution ---------
--------------------------

type LastSeen = Map Integer Integer
data SeqState = SeqState { ssLastSeen :: LastSeen, ssLastN :: Integer, ssLastTurn :: Integer }

theSequence :: [Integer] -> [Integer]
theSequence initial = initial ++ unfoldr next initialState
  where initialState = 
          SeqState (M.fromList $ zip initial [1..]) (last initial) (toInteger $ length initial)

        next ss@(SeqState {..}) = case ssLastSeen !? ssLastN of
          Nothing       -> let newVal = 0 
                            in Just (newVal, newState newVal ss)
          Just lastSeen -> let newVal = (ssLastTurn - lastSeen)
                            in Just (newVal, newState newVal ss)
          where currTurn = ssLastTurn + 1
                newState newVal (SeqState {..}) =
                  SeqState (M.insert ssLastN ssLastTurn ssLastSeen) newVal currTurn

---------- IO ------------
--------------------------

doDay15 :: IO ()
doDay15 = do
  -- let fp = "inputs/day15test.txt"
  let fp     = "inputs/day15.txt"
  input <- readFile fp
  let Just ns = parse input
  print $ theSequence ns !! (2020 - 1)
  print $ theSequence ns !! (30000000 - 1)
