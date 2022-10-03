module Tests.Interpreter where

import Data.Default (def)
import Data.String.Interpolate
import Error.Diagnose.Diagnostic (diagnosticToJson)
import Niveo.Interpreter (Context (..), interpret')
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase)
import Tests.Common

interpret'' :: Text -> Either Text Text
interpret'' got = interpret' ctx & bimap (decodeUtf8 . diagnosticToJson) show
  where
    ctx = Context {env = def, fin = "<test>", src = got}

assertExpr, assertExprError :: Text -> Text -> Assertion
assertExpr = assert' . interpret''
assertExprError = assertError' . interpret''

test_arithmetic :: TestTree
test_arithmetic =
  testGroup
    "Should evaluate unary expressions"
    [ testCase "with precedence" $
        "+2 / (9 -6 + 1  )- 3 *-5.7 ** 1 ** -6" `assertExpr` "17.6",
      testCase "with string concat" $
        [i|"Hello," + " world!"|] `assertExpr` [i|"Hello, world!"|],
      testCase "with simple `let`" $
        "let a = 9; a * 6" `assertExpr` "54",
      testCase "with simple `let`, undefined variable" $
        "let a = 9; b * 6" `assertExprError` [i|`b` not found in this scope|],
      testCase "with nested `let`" $
        [__i|
          let c = 38; let e = 1; let g = 2; 
          let a =
            let b =
              c +
                let d = e;
                d + 1;
            b;
          let f = a + g;
          f
        |]
          `assertExpr` "42",
      testCase "with `if-else`, `let`, blocks" $
        [__i|
          3000 + {
            let b = if (true && false) {
              114
            } else if (2 + 2 == 5) {
              -514
            } else {
              86
            };
            7000 + b
          }
        |]
          `assertExpr` "10086"
    ]

test_coll :: TestTree
test_coll =
  testGroup
    "Should evaluate collection literals and operations"
    [ testCase "with list concat" $
        "['what, 'is] + ['love]" `assertExpr` "['what, 'is, 'love]",
      testCase "with list indexing" $
        "[0, 1, 2, 3][3]" `assertExpr` "3",
      testCase "with list indexing, invalid index" $
        "[0, 1, 2, 3][4]" `assertExprError` "index out of bounds",
      testCase "with struct, no trailing comma" $
        [i|struct{"foo" + "bar" = 4.2, "ba z" = true && false}|]
          `assertExpr` [i|struct{"foobar" = 4.2, "ba z" = false}|],
      testCase "with struct, trailing comma, key shorthand, list" $
        [__i|
          let foo = '__some_foo__;
          struct{
            foo,
            baz: [1, 2.3],
          }
        |]
          `assertExpr` [i|struct{"foo" = '__some_foo__, "baz" = [1, 2.3]}|],
      testCase "with struct accessors" $
        [__i|
          let s = struct{one: "Night", 'blinding = "Lights"};
          s.one + s::blinding
        |]
          `assertExpr` [i|"NightLights"|],
      testCase "with struct accessors, invalid entry" $
        [i|struct{one: "Night"}.two|] `assertExprError` "no entry found"
    ]

test_bool :: TestTree
test_bool =
  testGroup
    "Should parse booleans"
    [ testCase "with comparisons" $
        "  -(-1+2) >=3 ==   ! !(4 <5+ (6/ 7))   " `assertExpr` "false",
      testCase "with `&&`, `||` and `!`" $
        [__i|
          let foo = 'foo;
          let bar = true;
          let a = !bar;
          let b = 3;
          foo == null || !!bar && a != (b == 3)
        |]
          `assertExpr` "true"
    ]

test_call :: TestTree
test_call =
  testGroup
    "Should evaluate function calls"
    [ testCase "with simple call" $
        "let abs = fun(a) {if (a >= 0) a else -a}; abs(-4) * 10 + abs(2)" `assertExpr` "42",
      testCase "with inline call, captures" $
        "let one = 1; fun(a) {a + one}(41)" `assertExpr` "42",
      testCase "with complex call" $
        "let add_c = fun(a) {fun(b) {a + b}}; add_c(40)(2)" `assertExpr` "42",
      testCase "with recursive definition" $
        "letrec fact = fun(a) {if (a <= 1) a else a * fact(a-1)}; fact(5)" `assertExpr` "120"
    ]
