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
import           Control.Arrow ((>>>))

--import Lib


type ID = Int
data Something = Something
  deriving (Show, Eq)

type SomethingElse = ()


--------------------------
-------- Parsing ---------
--------------------------

-- return the (earliest timestamp, bus ids)
parse :: String -> Maybe (Int, [ID])
parse = lines >>> \case
  [l1,l2] -> do earliest <- readMaybe l1
                busIds <- traverse readMaybe . filter (/="x") . splitOn "," $ l2
                Just (earliest, busIds)
  _ -> Nothing

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
  let testFp = "inputs/day13test.txt"
  let fp     = "inputs/day13.txt"
  input <- readFile fp
  let Just (earliest, busIds) = parse input

  print (earliest, busIds)
  --print $ part1 d
  --print $ part2 d

