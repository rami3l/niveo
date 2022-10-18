module Niveo.Utils (sepByComma, showRealFrac) where

import Witch

sepByComma :: Show a => [a] -> Text
sepByComma xs = into $ intercalate ", " (show <$> xs)

-- | If `n` is an `Integer`, then show it as an integer.
-- Otherwise it will be shown normally.
showRealFrac :: Double -> String
showRealFrac n = n & tryInto @Integer & either (const $ show n) show
