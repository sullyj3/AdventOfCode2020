{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE BlockArguments #-}
module Day15 where

-- delete unneeded stuff
import Data.List.Split (splitWhen)
-- import           Data.Maybe (mapMaybe)
-- import           Data.Foldable (foldl', find)
-- import           Data.List (sort)
import           Text.Read (readMaybe)
-- import           Text.Printf         (printf)
-- import qualified Data.Set            as S
-- import           Data.Set            (Set, (\\))
-- import qualified Data.Map as M
-- import           Data.Map (Map, (!?))

-- import Lib


data Something = Something
  deriving (Show, Eq)

type SomethingElse = ()


--------------------------
-------- Parsing ---------
--------------------------

parse :: String -> Maybe [Int]
parse = traverse readMaybe . splitWhen (==',')

--------------------------
-------- Part 1 ----------
--------------------------

part1 :: [Int] -> Int
part1 = undefined

--------------------------
-------- Part 2 ----------
--------------------------

part2 :: [Int] -> SomethingElse
part2 = undefined

--------------------------
---------- IO ------------
--------------------------

doDay15 :: IO ()
doDay15 = do
  let testFp = "inputs/day15test.txt"
  let fp     = "inputs/day15.txt"
  error "not yet implemented"
  input <- readFile fp
  let Just d = parse input
  print $ part1 d
  print $ part2 d

