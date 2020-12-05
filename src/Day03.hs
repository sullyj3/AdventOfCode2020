{-# LANGUAGE LambdaCase #-}

module Day03 (
  doDay3
  ) where


import Data.Maybe (fromJust)

data Cell = Empty | Tree
  deriving (Show, Eq)

parseInput :: String -> [[Cell]]
parseInput s = fromJust $ traverse (traverse charToCell) $ lines s

slope :: (Int, Int) -> [[Cell]] -> [Cell]
slope (right, down) grid = zipWith (!!) (map cycle $ eachNth down grid) indexes
  where indexes = map (*right) [1..]


eachNth :: Int -> [a] -> [a]
eachNth n xs = go n xs
  where go 0 (x:xs) = x : go (n-1) xs
        go a (_:xs) =     go (a-1) xs
        go _ []     = []

countTrees :: [Cell] -> Int
countTrees = length . filter (== Tree)

charToCell :: Char -> Maybe Cell
charToCell = \case
  '.' -> Just Empty
  '#' -> Just Tree
  _   -> Nothing

doDay3 :: IO ()
doDay3 = do
  part2

part1 :: IO ()
part1 = do
  putStrLn "Part 1:"
  putStrLn "======="

  grid <- parseInput <$> readFile "inputs/day3.txt"
  print $ countTrees . slope (3,1) $ grid

part2 :: IO ()
part2 = do
  putStrLn "Part 1:"
  putStrLn "======="

  grid <- parseInput <$> readFile "inputs/day3.txt"

  let slopes = [(1,1), (3,1), (5,1), (7,1), (1,2)]
  print $ product $ map (countTrees . flip slope grid) slopes
  

testDay3 :: IO ()
testDay3 = do
  grid <- parseInput <$> readFile "inputs/day3_example.txt"
  traverse print grid
  putStrLn "slope"
  print $ slope (3,1) grid
