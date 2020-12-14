module Day06 (
  doDay6
  ) where

import Data.List.Split (splitOn)
import qualified Data.Set as S
import           Data.Set (Set)

part2 :: [[String]] -> Int
part2 groups = let
  sets :: [[Set Char]]
  sets = map (map S.fromList) groups

  counts :: [Int]
  counts = map (S.size . (foldl1 S.intersection)) sets
  in sum counts

parse :: String -> [[String]]
parse = splitOn [""] . lines

doDay6 :: IO ()
doDay6 = readFile "inputs/day6.txt" >>= (print . part2 . parse)
