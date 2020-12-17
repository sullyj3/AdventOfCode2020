{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE BlockArguments #-}
module Day13 where

-- delete unneeded stuff
-- import           Data.Maybe (mapMaybe)
-- import           Data.Foldable (foldl', find)
-- import           Data.List (sort)
-- import           Text.Read (readMaybe)
-- import           Text.Printf         (printf)
-- import qualified Data.Set            as S
-- import           Data.Set            (Set, (\\))
-- import qualified Data.Map as M
-- import           Data.Map (Map, (!?))

import Lib


data Something = Something
  deriving (Show, Eq)

type SomethingElse = ()


--------------------------
-------- Parsing ---------
--------------------------

parse :: String -> Maybe Something
parse = undefined

--------------------------
-------- Part 1 ----------
--------------------------

part1 :: Something -> SomethingElse
part1 = undefined

--------------------------
-------- Part 2 ----------
--------------------------

part2 :: Something -> SomethingElse
part2 = undefined

--------------------------
---------- IO ------------
--------------------------

doDay13 :: IO ()
doDay13 = do
  let testFp = "inputs/dayntest.txt"
  let fp     = "inputs/dayn.txt"
  error "not yet implemented"
  input <- readFile fp
  let Just d = parse input
  print $ part1 d
  print $ part2 d

