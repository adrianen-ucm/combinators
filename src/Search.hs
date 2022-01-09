-- |
-- Module      : Search
-- Copyright   : (c) Adrián Enríquez Ballester, 2021
--
-- Utilities for searching combinators.
module Search
  ( Predicate (..),
    combSpace,
    search,
  )
where

import           Comb       (Comb (..), CombBase, size)
import           Data.Maybe (fromMaybe)

class Predicate a where
  check :: Int -> a -> Maybe Bool

combSpace :: (Enum a, Bounded a) => [Comb a]
combSpace =
  let ls = Base <$> [minBound .. maxBound]
   in ls <> go 2 ls
  where
    go n ss =
      let cs =
            [ App x y
              | x <- ss,
                y <- ss,
                size x + size y == n
            ]
       in cs <> go (n + 1) (ss <> cs)

-- | Example: @search 100 $ \e -> e :=: App e e@
search ::
  ( CombBase a,
    Ord a,
    Enum a,
    Bounded a,
    Predicate p
  ) =>
  Int ->
  (Comb a -> p) ->
  [Comb a]
search l q = filter pr combSpace
  where
    pr = fromMaybe False . check l . q
