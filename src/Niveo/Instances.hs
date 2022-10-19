{-# OPTIONS_GHC -Wno-orphans #-}

module Niveo.Instances () where

import Data.Vector (Vector)
import Error.Diagnose.Compat.Megaparsec (HasHints (..))
import Witch

instance {-# OVERLAPPABLE #-} HasHints Void msg where
  hints _ = mempty

instance From (Vector a) (Seq a) where
  from = fromList . toList
