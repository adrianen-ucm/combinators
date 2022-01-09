-- |
-- Module      : Comb
-- Copyright   : (c) Adrián Enríquez Ballester, 2021
--
-- This module defines a data type for combinators
-- and some utilities for managing them.
module Comb
  ( Comb (..),
    CombBase (..),
    size,
    apply,
  )
where

data Comb a
  = Base a
  | App (Comb a) (Comb a)
  deriving (Eq, Ord)

instance Show a => Show (Comb a) where
  show (Base x) = show x
  show (App x y) = parens x <> " " <> parens y
    where
      parens s = "(" <> show s <> ")"

class CombBase a where
  rule :: Comb a -> Maybe (Comb a)

size :: Comb a -> Int
size (Base _)  = 1
size (App x y) = size x + size y

apply :: CombBase a => Comb a -> Maybe (Comb a)
apply b@(Base _) = rule b
apply c@(App x y) =
  case (rule c, apply x, apply y) of
    (Just c', _, _) -> Just c'
    (_, Just x', _) -> Just $ App x' y
    (_, _, Just y') -> Just $ App x y'
    _               -> Nothing
