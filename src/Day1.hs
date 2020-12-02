module Day1 (
  doDay1
  ) where

import Lib

doDay1 :: IO ()
doDay1 = do
  putStrLn "Day 1"
  putStrLn "====="

  ns <- intList <$> readFile "inputs/day1.txt"

  print $ part1 ns
  print $ part2 ns

part1 :: [Int] -> Int
part1 ns = head [ a*b | a <- ns, b <- ns, a+b == 2020 ]

part2 :: [Int] -> Int
part2 ns = head [ a*b*c | a <- ns,
                          b <- ns,
                          c <- ns,
                          a+b+c == 2020 ]

