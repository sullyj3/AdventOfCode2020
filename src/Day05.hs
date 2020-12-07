{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}

module Day05 (
  doDay5
  ) where

import           Data.Maybe          (fromJust)
import           Text.Printf         (printf)
import           Data.Foldable
import           Data.List
import qualified Data.Set            as S
import           Data.Set            (Set)
import qualified Data.Map.Strict     as M
import           Data.Map.Strict     (Map)
import           Control.Monad
import           Control.Monad.Extra (whenM)


type BitString = [Bool]

type Plane = Set Int
type SeatID = Int
type Row = Int
type Col = Int

getSeatId :: Row -> Col -> SeatID
getSeatId row col = row * 8 + col

seatRow :: SeatID -> Row
seatRow = (`div` 8)

seatCol :: SeatID -> Col
seatCol = (`mod` 8)

getRow :: Row -> Set SeatID
getRow r = S.map (+ (r*8)) $ S.fromList [0..7]

planeRow :: Plane -> Row -> Set SeatID
planeRow plane r = S.intersection (getRow r) plane

showSeat :: SeatID -> String
showSeat s = printf "id: %d, row: %d, col: %d" s (seatRow s) (seatCol s)


bitStringInt :: [Bool] -> Int
bitStringInt bs = let oneZeroes = map (\b -> if b then 1 else 0) bs
  in sum $ zipWith (\bit pow -> bit*(2^pow))
                   (reverse $ oneZeroes)
                   [0..]


parseBit :: String -> Maybe Bool
parseBit = \case
  "F" -> Just False
  "B" -> Just True
  "L" -> Just False
  "R" -> Just True
  _   -> Nothing


parseBitString :: String -> Maybe BitString
parseBitString = traverse (parseBit . (:[]))


pathToRowCol :: String -> (Int, Int)
pathToRowCol p = let (rPath, cPath) = splitAt 7 p
                     (row, col) = ( bitStringInt . fromJust . parseBitString $ rPath
                                  , bitStringInt . fromJust . parseBitString $ cPath )
                  in (row, col)

doDay5 :: IO ()
doDay5 = do
  part1

part1 :: IO ()
part1 = do
  let fp = "inputs/day5.txt"
  rowCols <- map pathToRowCol . lines <$> readFile fp
  let seatIds = uncurry getSeatId <$> rowCols
  print $ maximum seatIds






testCases = [ ("BFFFBBFRRR", 70,  7, 567)
            , ("FFFBBBFRRR", 14,  7, 119)
            , ("BBFFBBFRLL", 102, 4, 820)
            ]

