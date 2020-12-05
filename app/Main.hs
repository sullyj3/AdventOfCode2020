module Main where

import Text.Printf (printf)
import Data.Foldable (traverse_, for_)
import System.IO
import System.Environment (getArgs)
import Text.Read (readMaybe)

import Day01
import Day02
import Day03
import Day04
import Day05
import Day06
import Day07
import Day08
import Day09
import Day10
import Day11
import Day12
import Day13
import Day14
import Day15
import Day16
import Day17
import Day18
import Day19
import Day20
import Day21
import Day22
import Day23
import Day24
import Day25

currentDay = doDay4

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> currentDay
    [dayNumber] -> case readMaybe dayNumber of
      Just 1 -> doDay1
      Just 2 -> doDay2
      Just 3 -> doDay3
      Just 4 -> doDay4
      Just _ -> putStrLn "I haven't implemented that day yet!"
      Nothing -> putStrLn "That's not a number!"
    _ -> putStrLn "Please enter a day number!"
