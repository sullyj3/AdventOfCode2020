{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE BlockArguments #-}

module Day08 (
  doDay8
  ) where

import Text.Read (readMaybe)
import           Control.Monad.State
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Foldable

data Instruction = ACC Int | NOP Int | JMP Int
  deriving (Show)


data ProgState = PS { progStateInstructIx :: Int
                    , progStateAcc :: Int
                    , progStateInstructSeen :: Seq Bool }
type Program = Seq Instruction
data Termination = InfLoop | ReadPastEnd | InvalidJump
  deriving (Show, Eq)

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


-- return the final acc value and termination status
executeProg :: Program -> (Int, Termination)
executeProg prog = (acc, term)
  where (term, finalState) = runState (progLoop prog) $ (initProgState (Seq.length prog), [])
        (PS _ acc _, _) = finalState



-- keep a list of the intruction indices visited for debugging
progLoop :: Program -> State (ProgState,[Int]) Termination
progLoop prog = do
  (ps@(PS ix acc seen), ixs) <- get
  if | ix == progLen                -> pure ReadPastEnd
     | not . inRange 0 progLen $ ix -> pure InvalidJump
     | Seq.index seen ix            -> pure InfLoop
     | otherwise                    -> put (runStep prog ps, ix:ixs) >> progLoop prog
  where progLen = Seq.length prog
        -- half open interval
        inRange l r n = n >= l && n < r


runStep :: Program -> ProgState -> ProgState
runStep prog (PS ix acc seen) = let
  instruct = Seq.index prog ix
  in case instruct of
    ACC n -> PS (ix+1) (acc+n) (Seq.update ix True seen)
    NOP _ -> PS (ix+1) acc     (Seq.update ix True seen)
    JMP n -> PS (ix+n) acc     (Seq.update ix True seen)


findFaultyInstruction :: Program -> IO ()
findFaultyInstruction prog = do
  (flip Seq.traverseWithIndex) prog \i instruct -> case instruct of
    ACC _ -> pure ()
    _     -> testToggle i instruct
  putStrLn "done"
    
    where testToggle i instruct = do
            putStrLn $ "testing instruction " <> show i <> ": " <> show instruct
            let prog' = Seq.adjust' toggle i prog
                (_, term) = executeProg prog'
            print term
            if term == ReadPastEnd then putStrLn "\nSUCCESS!\n" else pure ()

          toggle (NOP n) = JMP n
          toggle (JMP n) = NOP n


doDay8 :: IO ()
doDay8 = do
  -- let fp = "inputs/day8.txt"
  let fp = "inputs/day8fixed.txt"
  Just prog <- parseProgram <$> readFile fp
  print $ executeProg prog


