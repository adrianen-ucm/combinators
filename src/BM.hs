-- |
-- Module      : BM
-- Copyright   : (c) Adrián Enríquez Ballester, 2021
--
-- Definition of the BM combinator.
module BM
  ( BM (..),
  )
where

import           Comb (Comb (App, Base), CombBase (..))

data BM
  = B
  | M
  deriving (Show, Eq, Ord, Enum, Bounded)

instance CombBase BM where
  rule (App (App (App (Base B) f) g) x) =
    Just $ App f (App g x)
  rule (App (Base M) x) =
    Just $ App x x
  rule _ = Nothing
