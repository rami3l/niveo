module Niveo.Interpreter.Std (stdMod) where

import Data.FileEmbed (embedStringFile)
import Data.Map.Strict qualified as Map

stdMod :: Text -> Maybe Text
stdMod = (stdMods Map.!?)

stdMods :: Map Text Text
stdMods =
  Map.fromList
    [ ("std/list", $(embedStringFile "assets/std/list.niveo"))
    ]
