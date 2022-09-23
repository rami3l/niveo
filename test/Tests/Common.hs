module Tests.Common where

import Data.String.Interpolate
import Relude
import Test.Tasty.HUnit (Assertion, (@?))
import Text.Regex.TDFA ((=~))

assertRegexMatch :: Text -> Text -> Assertion
assertRegexMatch got pattern' =
  (got =~ pattern' :: Bool)
    @? [i|regex mismatch: expected `#{pattern'}`, got `#{got}`|]