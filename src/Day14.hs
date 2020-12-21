{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE BlockArguments #-}
module Day14 where

-- delete unneeded stuff
-- import           Data.Maybe (mapMaybe)
-- import           Control.Arrow ((>>>))
import           Data.Foldable (foldl')
-- import           Data.List (stripPrefix)
-- import           Text.Read (readMaybe)
-- import           Text.Printf         (printf)
-- import qualified Data.Set            as S
-- import           Data.Set            (Set, (\\))
import qualified Data.Map as M
import           Data.Map (Map)
import Data.Word
import Data.Bits
import Data.Void

import Text.Megaparsec
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

import Numeric (showIntAtBase)
import Data.Char (intToDigit)
import Lib


type Program = [Command]

data Command = UpdateMask Word64 Word64 | MemWrite Word64 Word64
  deriving (Show, Eq)

type SomethingElse = ()



--------------------------
-------- Parsing ---------
--------------------------

-- make sure to treat extra bits above 36 correctly

type Parser = Parsec Void String

parseProg :: Parser Program
parseProg = parseCommand `sepEndBy` C.eol

parseCommand :: Parser Command
parseCommand = parseMemWrite <|> parseUpdateMask

parseMemWrite :: Parser Command
parseMemWrite = do
  C.string "mem["
  addr <- L.decimal
  C.string "] = "
  val <- L.decimal
  pure $ MemWrite addr val

parseUpdateMask :: Parser Command
parseUpdateMask = do
  C.string "mask = "
  s <- many (oneOf "10X")
  case bitMask s of
    Just masks -> pure masks
    Nothing    -> fail "Invalid mask string"

bitMask :: String -> Maybe Command
bitMask m = UpdateMask <$> setOnes <*> setZeroes where
  setOnes   =             (readBin $ replace 'X' '0' m)
  setZeroes = (maxBound .&.) <$> (readBin $ replace 'X' '1' m)

showBinary :: (Show a, Integral a) => a -> String
showBinary n = showIntAtBase 2 intToDigit n ""

--------------------------
-------- Part 1 ----------
--------------------------
type Memory = Map Word64 Word64
data ProgState = ProgState Word64 Word64 Memory

part1 :: Program -> Integer
part1 prog = sum $ toInteger <$> finalMem
  where ProgState _ _ finalMem = foldl' executeCmd initialState prog
        initialState = ProgState minBound maxBound mempty


applyMask :: Word64 -> Word64 -> Word64 -> Word64
applyMask setOnes setZeroes n = (n .|. setOnes) .&. setZeroes

executeCmd :: ProgState -> Command -> ProgState
executeCmd (ProgState setOnes setZeroes mem) = \case
  MemWrite addr val -> ProgState setOnes setZeroes
    $ M.insert addr (applyMask setOnes setZeroes val) mem
  UpdateMask setOnes' setZeroes' -> ProgState setOnes' setZeroes' mem



--------------------------
-------- Part 2 ----------
--------------------------

part2 :: Program -> SomethingElse
part2 = undefined

--------------------------
---------- IO ------------
--------------------------

doDay14 :: IO ()
doDay14 = do
  -- let fp = "inputs/day14test.txt"
  let fp     = "inputs/day14.txt"
  input <- readFile fp
  let Right prog = parse parseProg fp input
  print $ part1 prog
  -- print $ part2 d

