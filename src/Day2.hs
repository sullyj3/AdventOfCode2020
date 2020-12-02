module Day2 (
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

data Row = Row Policy String
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



rowValid :: Row -> Bool
rowValid (Row (Policy charMin charMax char) pass) = charCount <= charMax && charCount >= charMin
  where
    charCount = length . filter (==char) $ pass


countValidRows :: [Row] -> Int
countValidRows rows = length . filter rowValid $ rows


doDay2 :: IO ()
doDay2 = do
  putStrLn "Day 2"
  putStrLn "====="

  s <- readFile "inputs/day2.txt"
  case parse parseRows "day2.txt" s of
    Right rs -> do
      print rs
      print $ countValidRows rs
    Left _ -> putStrLn "The parser is wrong!"


-- ========================================================
-- testing:
-- ========================================================

cases = [ ("1-3 a: abcde", Row (Policy 1 3 'a') "abcde")
        , ("1-3 b: cdefg", Row (Policy 1 3 'b') "cdefg")
        , ("2-9 c: ccccccccc", Row (Policy 2 9 'c') "ccccccccc")
        ]

moreCases = [
    "1-8 n: dpwpmhknmnlglhjtrbpx"
  , "11-12 n: frpknnndpntnncnnnnn"
  , "4-8 t: tmttdtnttkr"
  , "12-18 v: vvvvvvvqvvvvvqvvgf"
  , "3-4 c: cccc"
  , "17-18 z: zzzzzzzzdzzzzzzgzr"
  , "5-6 l: llltzl"
  , "4-5 g: fzfng"
  , "3-6 b: bjsbbxbb"
  , "4-5 b: dbbbl"
  ]

testParseRows :: IO ()
testParseRows = do
  print $ all (uncurry checkMatch) cases
  where
    checkMatch :: String -> Row -> Bool
    checkMatch s r =
      case parse parseRow "afile" s of
        Left _ -> False
        Right parsedRow -> parsedRow == r


testRowValid :: IO ()
testRowValid = do
  print numValid
  where numValid = length . filter rowValid $ rows
        rows = catMaybes $ parseMaybe parseRow <$> moreCases
