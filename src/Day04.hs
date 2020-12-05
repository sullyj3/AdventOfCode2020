{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE BlockArguments #-}

module Day04 (
  doDay4
  ) where

import           Text.Megaparsec hiding (count)
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Error as E

import Data.Void

import Control.Applicative ((<|>))
import Control.Monad
import Data.Foldable

import Data.Char (isSpace)
import Data.Either (partitionEithers)
import Data.List.Split (splitOn)
import           Data.List ( foldl'
                           , intercalate
                           , isPrefixOf
                           , intersect
                           , sort)
import Lib


type Passport = [Pair]

data Pair = BYR Int | IYR Int | EYR Int | HGT Height | HCL HairColor | ECL EyeColor | PID String | CID
  deriving (Eq, Show)

type HairColor = String

data EyeColor = Amber | Blue | Brown | Gray | Green | Hazel | Other
  deriving (Eq, Show)

data Height = Cm Int | Inches Int
  deriving (Show, Eq)


doDay4 :: IO ()
doDay4 = do
  part2

part2 :: IO ()
part2 = do
  let fp = "inputs/day4.txt"
  passports <- splitOn "\n\n" . trim <$> readFile fp
  let (lefts, rights) = partitionEithers . map (parse parsePassport fp) $ passports

  print $ count hasRequiredKeys rights


trim :: String -> String
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

hasRequiredKeys :: Passport -> Bool
hasRequiredKeys p = requiredKeys `allIn` keySet p
  where requiredKeys = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

        keySet :: Passport -> [String]
        keySet = map pairKey

-- use infix: xs `allIn` ys
allIn :: (Ord a, Eq a) => [a] -> [a] -> Bool
allIn xs ys = sort xs == sort (xs `intersect` ys)

-- -----------------
-- Parsing
-- -----------------
type Parser = Parsec Void String

spaceorEol :: Parser ()
spaceorEol = () <$ C.char ' ' <|> () <$ C.eol


parsePassport :: Parser Passport
parsePassport = do
  sepBy1 parsePair spaceorEol


parsePassports :: Parser [Passport]
parsePassports = do
  sepBy1 parsePassport (replicateM 2 C.eol)
  

pairKey :: Pair -> String
pairKey = \case
  BYR _ -> "byr"
  IYR _ -> "iyr"
  EYR _ -> "eyr"
  HGT _ -> "hgt"
  HCL _ -> "hcl"
  ECL _ -> "ecl"
  PID _ -> "pid"
  CID   -> "cid"


parsePair :: Parser Pair
parsePair = asum [parseByr, parseIyr, parseEyr, parseHgt, parseHcl, parseEcl, parsePid, parseCid]

makeKvPairParser :: String -> Parser Pair -> Parser Pair
makeKvPairParser k parseVal = do
  C.string k
  C.char ':'
  parseVal

parseYear :: Parser Int
parseYear = read <$> replicateM 4 C.digitChar


parseBirthyear = do
  y <- parseYear
  if | y < 1920 -> fail "Birth year too early!"
     | y > 2002 -> fail "Birth year too late!"
     | otherwise -> pure y


parseIssueYear = do
  y <- parseYear
  if | y < 2010 -> fail "Issue year too early!"
     | y > 2020 -> fail "Issue year too late!"
     | otherwise -> pure y


parseExpirYear = do
  y <- parseYear
  if | y < 2020 -> fail "Expiry year too early!"
     | y > 2030 -> fail "Expiry year too late!"
     | otherwise -> pure y


parseByr = makeKvPairParser "byr" $ BYR <$> parseBirthyear
parseIyr = makeKvPairParser "iyr" $ IYR <$> parseIssueYear
parseEyr = makeKvPairParser "eyr" $ EYR <$> parseExpirYear
parseHgt = makeKvPairParser "hgt" $ parseHeight
parseHcl = makeKvPairParser "hcl" $ HCL <$> parseHairColor
parseEcl = makeKvPairParser "ecl" $ ECL <$> parseEyeColor
parsePid = makeKvPairParser "pid" $ pidVal
parseCid = makeKvPairParser "cid" $ cidVal


parseHairColor :: Parser HairColor
parseHairColor = C.char '#' *> replicateM 6 C.hexDigitChar


parseEyeColor :: Parser EyeColor
parseEyeColor = asum [ Amber <$ C.string "amb"
                     , Blue  <$ C.string "blu"
                     , Brown <$ C.string "brn"
                     , Gray  <$ C.string "gry"
                     , Green <$ C.string "grn"
                     , Hazel <$ C.string "hzl"
                     , Other <$ C.string "oth"
                     ]


parseHeight :: Parser Pair
parseHeight = do
  h <- L.decimal

  HGT <$> ((C.string "cm" *> cm h) <|> (C.string "in" *> inches h))

  where cm n     | n > 193   = fail "cm too high!"
                 | n < 150   = fail "cm too low!"
                 | otherwise = pure $ Cm n
        inches n | n > 76    = fail "inches too high!"
                 | n < 59    = fail "inches too low!"
                 | otherwise = pure $ Inches n


pidVal :: Parser Pair
pidVal = PID <$> replicateM 9 C.digitChar


cidVal :: Parser Pair
cidVal = CID <$ (some $ satisfy (not . isSpace))


-- -----------------
-- Testing
-- -----------------


testPart2 :: IO ()
testPart2 = do
  putStrLn "checking invalid examples fail:"
  printResults "inputs/day4_invalid_examples.txt"
  putStrLn "checking valid examples succeed:"
  printResults "inputs/day4_valid_examples.txt"
  where printResults :: FilePath -> IO ()
        printResults fp = do
          passports <- splitOn "\n\n" . trim <$> readFile fp
          let results = (parse parsePassport fp) <$> passports

          for_ results \res ->
            either (\l -> putStrLn (E.errorBundlePretty l))
                   (\r -> print r)
                   res
          putStrLn "\n"
