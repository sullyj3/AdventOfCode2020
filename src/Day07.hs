module Day07 (
  doDay7
  ) where

import Data.Void
import Text.Megaparsec
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

import Data.Foldable (traverse_)
import qualified Data.Map as M
import           Data.Map (Map, (!))
import qualified Data.Set as S
import           Data.Set (Set)

import Debug.Trace (trace)

data Rule = Rule { ruleBagType :: String
                 , ruleContents :: [(Int, String)]
                 }
  deriving Show


-- Based on the containment rules, build a dag representing the possible bag types
-- that each bag type could be directly contained by. Represented as a Map
-- from bag type to set of types that can immediately contain it.
containedByDag :: [Rule] -> Map String (Set String)
containedByDag rules = foldr addrule mempty rules
  where addrule :: Rule -> Map String (Set String) -> Map String (Set String)
        addrule (Rule ty contents) acc = foldr f acc (snd <$> contents)
          where
            f :: String -> Map String (Set String) -> Map String (Set String)
            f = (\c acc' -> M.insertWith S.union c (S.singleton ty) acc')


-- the set of bag types that can transitively contain the given bag type
canContain :: String -> Map String (Set String) -> Set String
canContain bagType containedByMap
  | immediatelyContainedBy == mempty = mempty
  | otherwise = result
  where
      immediatelyContainedBy = M.findWithDefault mempty bagType containedByMap
      result = foldr (\ty set -> if ty `S.member` set
                                 -- if it's a member, we must have already found and added
                                 -- all its transitive containers, so skip it
                                 then set
                                 else set <> S.insert ty (canContain ty containedByMap))
                     mempty
                     immediatelyContainedBy


doDay7 :: IO ()
doDay7 = do
  -- let fp = "inputs/day7test.txt"
  let fp = "inputs/day7.txt"
  input <- readFile fp
  case parse parseRules fp input of
    Left e -> error $ "bad parser: " <> show e
    Right rules ->
      do let dag = containedByDag rules
         print . S.size $ canContain "shiny gold" dag


-- parsing
type Parser = Parsec Void String

lexeme :: Parser a -> Parser a
lexeme p = L.lexeme (() <$ C.char ' ') p

parseBagCount :: Parser (Int, String)
parseBagCount = do
  n <- lexeme L.decimal
  ty <- parseBagType
  C.string "bags" <|> C.string "bag"
  pure (n, ty)

parseBagType :: Parser String
parseBagType = do
  w1 <- lexeme $ some C.letterChar
  w2 <- lexeme $ some C.letterChar
  pure $ w1 ++ " " ++ w2

parseRule :: Parser Rule
parseRule = do
  ty <- parseBagType
  lexeme $ C.string "bags contain"
  contents <- (parseBagCount `sepBy1` C.string ", ") <|> ([] <$ C.string "no other bags")
  C.char '.'
  pure $ Rule ty contents

parseRules :: Parser [Rule]
parseRules = do
  rules <- parseRule `sepEndBy` C.eol
  eof
  pure rules

-- debugging

showMap :: (Show k, Show v) => Map k v -> String
showMap = unlines . ("map: ":) . map show . M.toList


