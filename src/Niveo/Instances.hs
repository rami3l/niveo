{-# OPTIONS_GHC -Wno-orphans #-}

module Niveo.Instances () where

import Error.Diagnose.Compat.Megaparsec (HasHints (..))

instance {-# OVERLAPPABLE #-} HasHints Void msg where
  hints _ = mempty
