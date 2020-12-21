{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE BlockArguments #-}
module Day14 where

-- delete unneeded stuff
-- import           Data.Maybe (mapMaybe)
-- import           Control.Arrow ((>>>))
import           Data.Foldable (foldl')
import           Control.Monad (replicateM)
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

data Command = UpdateMask Mask | MemWrite Word64 Word64
  deriving (Show, Eq)



--------------------------
-------- Parsing ---------
--------------------------

-- make sure to treat extra bits above 36 correctly

type Parser = Parsec Void String

parseProg :: Parser Program
parseProg = parseCommand `sepEndBy` C.eol

parseCommand :: Parser Command
parseCommand = parseUpdateMask <|> parseMemWrite

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
  UpdateMask <$> replicateM 36 (oneOf "10X")

showBinary :: (Show a, Integral a) => a -> String
showBinary n = showIntAtBase 2 intToDigit n ""

--------------------------
-------- Part 1 ----------
--------------------------
type Memory = Map Word64 Word64
data ProgState1 = ProgState1 { setOnes :: Word64, setZeroes :: Word64, psMem :: Memory}

splitMask :: Mask -> Maybe (Word64, Word64)
splitMask m = (,) <$> setOnes <*> setZeroes where
  setOnes   =                    (readBin $ replace 'X' '0' m)
  setZeroes = (maxBound .&.) <$> (readBin $ replace 'X' '1' m)

part1 :: Program -> Integer
part1 prog = sum $ toInteger <$> finalMem
  where ProgState1 _ _ finalMem = foldl' executeCmd1 initialState prog
        initialState = ProgState1 minBound maxBound mempty

applyMask :: Word64 -> Word64 -> Word64 -> Word64
applyMask setOnes setZeroes n = (n .|. setOnes) .&. setZeroes

executeCmd1 :: ProgState1 -> Command -> ProgState1
executeCmd1 (ProgState1 setOnes setZeroes mem) = \case
  MemWrite addr val -> ProgState1 setOnes setZeroes
    $ M.insert addr (applyMask setOnes setZeroes val) mem
  UpdateMask mask' -> case splitMask mask' of
    Just (setOnes', setZeroes') -> ProgState1 setOnes' setZeroes' mem
    Nothing -> error $ "encountered invalid mask: " <> mask'

--------------------------
-------- Part 2 ----------
--------------------------

-- 1s, 0s, and Xs
type Mask = String

data ProgState2 = ProgState2 { mask :: Mask, mem :: Memory }

allMasks :: Mask -> [Mask]
allMasks m = traverse replaceX m
  where replaceX :: Char -> [Char]
        replaceX 'X' = "10"
        replaceX c = pure c

part2 :: Program -> Integer
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

