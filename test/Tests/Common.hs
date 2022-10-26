module Tests.Common (assertRegexMatch, assert', assertError') where

import Data.String.Interpolate
import Test.Tasty.HUnit (Assertion, (@?), (@?=))
import Text.Regex.TDFA ((=~))

assertRegexMatch :: Text -> Text -> Assertion
assertRegexMatch got pattern' =
  (got =~ pattern' :: Bool)
    @? [i|regex mismatch: expected `#{pattern'}`, got `#{got}`|]

assert' :: (Show a, Eq a) => Either Text a -> a -> Assertion
assert' (Right got) expected = got @?= expected
assert' (Left e) _ = error [i|test failed: got `#{e}`|]

assertError' :: Show a => Either Text a -> Text -> Assertion
assertError' (Right a) pattern' = error [i|test did not fail as expected: expected `#{pattern'}`, got `#{show @String a}`|]
assertError' (Left got) pattern' = got `assertRegexMatch` pattern'
