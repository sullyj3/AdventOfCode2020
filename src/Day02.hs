module Day02 (
  doDay2
  ) where

import Data.Void

import Text.Megaparsec
import qualified Text.Megaparsec.Char as C
import Data.Maybe (catMaybes)


data Policy = Policy { charcountMin :: Int
                     , charcountMax :: Int
                     , polChar :: Char
                     }
  deriving (Show, Eq)

data Row = Row { rowPolicy :: Policy, rowPass :: String }
  deriving (Show, Eq)

type Parser = Parsec Void String


parseRows :: Parser [Row]
parseRows = many (parseRow <* C.char '\n')


parseRow :: Parser Row
parseRow = do
  pol <- parsePolicy
  C.string ": "
  pass <- many C.letterChar
  pure $ Row pol pass


parseInt :: Parser Int
parseInt = read <$> some C.digitChar


parsePolicy :: Parser Policy
parsePolicy = do
  min <- parseInt
  C.char '-'
  max <- parseInt
  C.char ' '
  c <- C.letterChar
  pure $ Policy min max c


xor :: Bool -> Bool -> Bool
xor = (/=)

rowValidPart1 :: Row -> Bool
rowValidPart1 (Row (Policy charMin charMax char) pass) = charCount <= charMax && charCount >= charMin
  where
    charCount = length . filter (==char) $ pass

rowValidPart2 :: Row -> Bool
rowValidPart2 (Row (Policy charMin charMax char) pass) = case (index pass charMin, index pass charMax) of
  (Just c, Just d) -> (c == char) `xor` (d == char)
  _ -> False


countValidRows :: (Row -> Bool) -> [Row] -> Int
countValidRows p rows = length . filter p $ rows


row1 = Row (Policy 1 3 'a') "abcde"

index :: Eq a => [a] -> Int -> Maybe a
index (x:_)  1 = Just x
index (_:xs) n = index xs (n-1)
index _ _ = Nothing


doDay2 :: IO ()
doDay2 = do
  pure ()
  putStrLn "Day 2"
  putStrLn "====="
  s <- readFile "inputs/day2.txt"
  let rows = case parse parseRows "day2.txt" s of
        Right rs -> rs
        Left e -> error (show e)

  putStrLn "Part 1:"
  print $ countValidRows rowValidPart1 rows

  putStrLn "Part 2:"
  print $ countValidRows rowValidPart2 rows


