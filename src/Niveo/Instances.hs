{-# OPTIONS_GHC -Wno-orphans #-}

module Niveo.Instances () where

import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Error.Diagnose.Compat.Megaparsec (HasHints (..))
import Witch

instance {-# OVERLAPPABLE #-} From [a] (Vector a) where
  from = Vector.fromList

instance {-# OVERLAPPABLE #-} HasHints Void msg where
  hints _ = mempty
