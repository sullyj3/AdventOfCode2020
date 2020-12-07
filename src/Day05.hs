{-# LANGUAGE LambdaCase #-}

module Day05 (
  doDay5
  ) where

import           Data.Maybe          (fromJust)
import           Text.Printf         (printf)
import qualified Data.Set            as S
import           Data.Set            (Set, (\\))


type BitString = [Bool]

type Plane = Set SeatID
type SeatID = Int
type Row = Int
type Col = Int

seatId :: Row -> Col -> SeatID
seatId row col = row * 8 + col

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


passToRowCol :: String -> (Int, Int)
passToRowCol p = let (rowPath, colPath) = splitAt 7 p
                     (row, col) = ( bitStringInt . fromJust . parseBitString $ rowPath
                                  , bitStringInt . fromJust . parseBitString $ colPath )
                  in (row, col)

doDay5 :: IO ()
doDay5 = do
  rowCols <- map passToRowCol . lines <$> readFile "inputs/day5.txt"
  let seatIds = S.fromList $ uncurry seatId <$> rowCols

  print $ part1 seatIds
  print $ part2 seatIds

part1 :: Plane -> SeatID
part1 = maximum

part2 :: Plane -> SeatID
part2 plane = case S.toList matches of
  [myId] -> myId
  matches -> error $ "more than one match!\n" <> unlines (map showSeat matches)
  where
    possibleRows = [0..127]
    possibleCols = [0..7]
    possibleIds = S.fromList [seatId r c | r <- possibleRows, c <- possibleCols]
    missingIds = possibleIds \\ plane

    matches = S.filter p missingIds

    p :: SeatID -> Bool
    p sid 
      | r == 0 || r == 127 = False
      | planeRow plane (r-1) == mempty = False
      | planeRow plane (r+1) == mempty = False
      | otherwise = True
      where
        (r, _c) = (seatRow sid, seatCol sid)
