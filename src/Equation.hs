-- |
-- Module      : Equation
-- Copyright   : (c) Adrián Enríquez Ballester, 2021
--
-- Defines a combinator search condition in terms
-- of an equation.
module Equation where

import           Comb          (Comb, CombBase, apply)
import           Control.Monad (guard)
import           Data.Maybe    (isJust)
import qualified Data.Set      as S
import           Search        (Predicate (..))

data Equation a
  = Comb a :=: Comb a
  deriving (Show)

instance (CombBase a, Ord a) => Predicate (Equation a) where
  check l (c1 :=: c2) =
    checkEquation l (Just c1, Just c2) (mempty, mempty)

checkEquation ::
  (CombBase a, Ord a) =>
  Int ->
  (Maybe (Comb a), Maybe (Comb a)) ->
  (S.Set (Comb a), S.Set (Comb a)) ->
  Maybe Bool
checkEquation l (c1, c2) (v1, v2)
  | c1 == c2 = Just $ isJust c1
  | c1InC2 || c2InC1 = Just True
  | l == 0 = Nothing
  | otherwise = checkEquation l' (c1', c2') (v1', v2')
  where
    l' = max (-1) (l - 1)
    v1' = maybe v1 (`S.insert` v1) c1
    v2' = maybe v2 (`S.insert` v2) c2
    c1InC2 = maybe False (`S.member` v2) c1
    c2InC1 = maybe False (`S.member` v1) c2
    c1' = do
      c <- c1
      guard $ c `S.notMember` v1
      apply c
    c2' = do
      c <- c2
      guard $ c `S.notMember` v2
      apply c
