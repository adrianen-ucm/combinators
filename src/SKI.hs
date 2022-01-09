-- |
-- Module      : SKI
-- Copyright   : (c) Adrián Enríquez Ballester, 2021
--
-- Definition of the SKI combinator.
module SKI
  ( SKI (..),
  )
where

import           Comb (Comb (App, Base), CombBase (..))

data SKI
  = S
  | K
  | I
  deriving (Show, Eq, Ord, Enum, Bounded)

instance CombBase SKI where
  rule (App (App (App (Base S) m) n) o) =
    Just $ App (App m o) (App n o)
  rule (App (App (Base K) a) _) =
    Just a
  rule (App (Base I) x) =
    Just x
  rule _ = Nothing
