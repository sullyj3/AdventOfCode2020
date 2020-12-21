-- from https://rosettacode.org/wiki/Chinese_remainder_theorem#Haskell

module ChineseRemainder where

import Control.Monad (zipWithM)

egcd :: Integer -> Integer -> (Integer, Integer)
egcd _ 0 = (1, 0)
egcd a b = (t, s - q * t)
  where
    (s, t) = egcd b r
    (q, r) = a `quotRem` b
 
modInv :: Integer -> Integer -> Either String Integer
modInv a b =
  case egcd a b of
    (x, y)
      | a * x + b * y == 1 -> Right x
      | otherwise ->
        Left $ "No modular inverse for " ++ show a ++ " and " ++ show b
 
chineseRemainder :: [Integer] -> [Integer] -> Either String Integer
chineseRemainder residues modulii =
  zipWithM modInv crtModulii modulii >>=
  (Right . (`mod` modPI) . sum . zipWith (*) crtModulii . zipWith (*) residues)
  where
    modPI = product modulii
    crtModulii = (modPI `div`) <$> modulii
