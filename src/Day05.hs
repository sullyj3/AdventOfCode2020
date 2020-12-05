{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}

module Day05 (
  doDay5
  ) where

import Data.Maybe (fromJust)
import Text.Printf (printf)
import Data.Foldable

data Region = Reg { regStart :: Int
                  , regLen   :: Int } deriving (Show, Eq)

data Side = Front | Back | SLeft | SRight deriving (Show, Eq)


type Path = [Side]


parseSide :: String -> Maybe Side
parseSide = \case
  "F" -> Just Front
  "B" -> Just Back
  "L" -> Just SLeft
  "R" -> Just SRight
  _   -> Nothing

parsePathUnsafe :: String -> Path
parsePathUnsafe = fromJust . parsePath


parsePath = traverse (parseSide . (:[]))


getSeatId :: Int -> Int -> Int
getSeatId row col = row * 8 + col


bSearch :: Int -> Path -> Int
bSearch initialSize path = n
  where
    Reg n _ = foldl' narrow (Reg 0 initialSize) path

narrow :: Region -> Side -> Region
narrow (Reg {regStart, regLen}) = \case
  Front  -> Reg regStart halfLen
  Back   -> Reg (regStart + halfLen) halfLen
  SLeft  -> Reg regStart halfLen
  SRight -> Reg (regStart + halfLen) halfLen
  where
    halfLen = regLen `div` 2


getRow :: Path -> Int
getRow = bSearch 128


getCol :: Path -> Int
getCol = bSearch 8


doDay5 :: IO ()
doDay5 = do
  testPart1
  pure ()

part1 = do
  let fp = "inputs/day5.txt"
  paths <- fromJust . traverse parsePath . lines <$> readFile fp
  print $ maximum $ map (\p -> getSeatId (getRow p) (getCol p)) paths


-- --------------
-- Testing
-- --------------

testPart1 :: IO ()
testPart1 = do
  for_ testCases checkCase
  where
    checkCase :: (String, Int, Int, Int) -> IO ()
    checkCase testCase@(path, expectedRow, expectedCol, expectedSeatId) = do
      putStrLn $ "testcase: " <> show testCase
      printf "result: row - %d, col - %d, id %d\n" row col sId
      print $ (row, col, sId) == (expectedRow, expectedCol, expectedSeatId)
      putStrLn "\n"
      where path' = fromJust $ traverse (parseSide . (:[])) path
            (rowPath,colPath) = splitAt 7 path'
            (row, col) = (getRow rowPath, getCol colPath)
            sId = getSeatId row col




testCases = [ ("BFFFBBFRRR", 70,  7, 567)
            , ("FFFBBBFRRR", 14,  7, 119)
            , ("BBFFBBFRLL", 102, 4, 820)
            ]

