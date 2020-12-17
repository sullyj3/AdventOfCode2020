{-# LANGUAGE LambdaCase #-}
module Cardinal where


data Cardinal = N | S | E | W
  deriving (Show, Eq)


turn90CCW :: Cardinal -> Cardinal
turn90CCW = \case
  N -> W
  W -> S
  S -> E
  E -> N


unit :: Cardinal -> (Int, Int)
unit = \case
  N -> ( 0, 1)
  S -> ( 0,-1)
  E -> ( 1, 0)
  W -> (-1, 0)
