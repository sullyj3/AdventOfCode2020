module Day08 (
  doDay8
  ) where

import Text.Read (readMaybe)
import           Control.Monad.State
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq

data Instruction = ACC Int | NOP Int | JMP Int
  deriving (Show)


data ProgState = PS { progStateInstructIx :: Int
                    , progStateAcc :: Int
                    , progStateInstructSeen :: Seq Bool }
type Program = Seq Instruction

parseInstruction :: String -> Maybe Instruction
parseInstruction s = case words s of
  [iname, arg] -> case iname of
    "acc" -> ACC <$> parseArg arg
    "nop" -> NOP <$> parseArg arg
    "jmp" -> JMP <$> parseArg arg
  _ -> Nothing
  where parseArg :: String -> Maybe Int
        parseArg ('+':n)   = readMaybe n
        parseArg n@('-':_) = readMaybe n

parseProgram :: String -> Maybe Program
parseProgram = traverse parseInstruction . Seq.fromList . lines


initProgState sz = PS { progStateInstructIx = 0
                      , progStateAcc = 0
                      , progStateInstructSeen = Seq.replicate sz False }


accBeforeLoop :: Program -> Int
accBeforeLoop program = evalState loop $ initProgState (Seq.length program)
  where
    loop :: State ProgState Int
    loop = do
      ps@(PS ix acc seen) <- get
      if Seq.index seen ix
      then pure acc
      else modify (runStep program) >> loop


traceInstructIx :: Program -> [Int]
traceInstructIx = undefined



runStep :: Program -> ProgState -> ProgState
runStep prog (PS ix acc seen) = let
  instruct = Seq.index prog ix
  in case instruct of
    ACC n -> PS (ix+1) (acc+n) (Seq.update ix True seen)
    NOP _ -> PS (ix+1) acc     (Seq.update ix True seen)
    JMP n -> PS (ix+n) acc     (Seq.update ix True seen)

doDay8 :: IO ()
doDay8 = do
  let fp = "inputs/day8.txt"
  Just prog <- parseProgram <$> readFile fp
  part1 prog
  where
    part1 prog = do
      print $ accBeforeLoop prog

    part2 prog = do
      undefined
