module Day07 (
  doDay7
  ) where

import Data.Void
import Text.Megaparsec
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

import qualified Data.Map as M
import           Data.Map (Map, (!?))
import qualified Data.Set as S
import           Data.Set (Set)
import           Data.Tuple (swap)


data Rule = Rule { ruleBagType :: String
                 , ruleContents :: [(Int, String)]
                 }
  deriving Show

type BagType = String


-- Based on the containment rules, build a dag representing the possible bag types
-- that each bag type could be directly contained by. Represented as a Map
-- from bag type to set of types that can immediately contain it.
containedByDag :: [Rule] -> Map BagType (Set BagType)
containedByDag rules = foldr addrule mempty rules
  where addrule :: Rule -> Map String (Set String) -> Map String (Set String)
        addrule (Rule ty contents) acc = foldr f acc (snd <$> contents)
          where
            f :: String -> Map String (Set String) -> Map String (Set String)
            f = (\c acc' -> M.insertWith S.union c (S.singleton ty) acc')


-- the set of bag types that can transitively contain the given bag type
canContain :: BagType -> Map BagType (Set BagType) -> Set BagType
canContain bagType containedByMap
  | immediatelyContainedBy == mempty = mempty
  | otherwise = result
  where
      -- if a type isn't present as a key in the map, it can't be contained by
      -- any other type, so we're at the end
      immediatelyContainedBy = M.findWithDefault mempty bagType containedByMap
      result = foldr (\ty set -> if ty `S.member` set
                                 -- if it's a member, we must have already found and added
                                 -- all its transitive containers, so skip it
                                 then set
                                 else set <> S.insert ty (canContain ty containedByMap))
                     mempty
                     immediatelyContainedBy


mustContain :: [Rule] -> Map BagType (Map BagType Int)
mustContain rules = foldr addrule mempty rules
  where addrule :: Rule -> Map String (Map String Int) -> Map String (Map String Int)
        addrule (Rule ty contents) acc = M.insert ty (toMap contents) acc

        toMap :: [(Int, String)] -> Map String Int
        toMap = M.fromList . map swap


-- if the key doesn't exist in the map, it doesn't need to contain anything
transitiveCountContents :: BagType -> Map BagType (Map BagType Int) -> Int
transitiveCountContents ty ruleMap = maybe 0 countContents (ruleMap !? ty)
  where countContents :: Map BagType Int -> Int
        countContents = sum . M.mapWithKey (\ty count -> count + count * transitiveCountContents ty ruleMap)


doDay7 :: IO ()
doDay7 = do
  -- let fp = "inputs/day7test2.txt"
  let fp = "inputs/day7.txt"
  input <- readFile fp
  either (\e -> error $ "bad parser: " <> show e)
         part2
       $ parse parseRules fp input

part1 rules = do 
  let dag = containedByDag rules
  print . S.size $ canContain "shiny gold" dag

part2 rules = do
  let ruleMap = mustContain rules
  print $ transitiveCountContents "shiny gold" ruleMap



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


