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
import qualified Data.Set            as S
import           Data.Set            (Set, (\\))
import qualified Data.Map as M
import           Data.Map (Map)
import Data.Word
import Data.Bits
import Data.Void
import Data.Char  (digitToInt)
import Data.Maybe (listToMaybe, fromJust)
import Numeric    (readInt)

import Text.Megaparsec
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

import Numeric (showIntAtBase)
import Data.Char (intToDigit)
import Lib

type BitString = String
type Mask = String

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

parseBitString :: Parser BitString
parseBitString = do
  n <- L.decimal
  pure $ showBinary n

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

-- from https://stackoverflow.com/questions/5921573/convert-a-string-representing-a-binary-number-to-a-base-10-string-haskell
readBin :: Integral a => String -> Maybe a
readBin = fmap fst . listToMaybe . readInt 2 (`elem` "01") digitToInt

executeCmd1 :: ProgState1 -> Command -> ProgState1
executeCmd1 (ProgState1 setOnes setZeroes mem) = \case
  MemWrite addr val ->
        ProgState1 setOnes setZeroes
          $ M.insert addr (applyMask setOnes setZeroes val) mem
  UpdateMask mask' -> case splitMask mask' of
    Just (setOnes', setZeroes') -> ProgState1 setOnes' setZeroes' mem
    Nothing -> error $ "encountered invalid mask: " <> mask'

--------------------------
-------- Part 2 ----------
--------------------------

-- 1s, 0s, and Xs

-- this time, we just keep a list of all the masks represented by string.
data ProgState2 = ProgState2 { mask :: Mask, mem :: Memory }

allAddrs :: Mask -> Word64 -> Set Word64
allAddrs mask addr = S.fromList $ fromJust . traverse readBin $ traverse replaceX masked
  where strAddr = showBinary addr

        masked = reverse $ zipWith combineBit (reverse mask)
                                              (reverse strAddr ++ repeat '0')
        combineBit '0' n = n
        combineBit '1' n = '1'
        combineBit 'X' n = 'X'

        replaceX :: Char -> [Char]
        replaceX 'X' = "10"
        replaceX c = pure c

part2 :: Program -> Integer
part2 prog = sum $ toInteger <$> finalMem
  where ProgState2 _ finalMem = foldl' executeCmd2 initialState prog
        initialState = ProgState2 [] mempty

writeAll :: Set Word64 -> Word64 -> Memory -> Memory
writeAll addrs val mem = M.fromSet (const val) addrs <> mem

executeCmd2 :: ProgState2 -> Command -> ProgState2
executeCmd2 (ProgState2 mask mem) = \case
  MemWrite addr val -> ProgState2 mask
    $ writeAll (allAddrs mask addr) val mem
  UpdateMask mask' -> ProgState2 mask' mem

---------- IO ------------
--------------------------

doDay14 :: IO ()
doDay14 = do
  -- let fp = "inputs/day14test.txt"
  -- let fp = "inputs/day14test2.txt"
  let fp     = "inputs/day14.txt"
  input <- readFile fp
  -- parseTest parseProg input
  let Right prog = parse parseProg fp input
  -- print $ part1 prog
  print $ part2 prog

