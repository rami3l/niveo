module Tests.Interpreter where

import Data.Default (def)
import Data.Map.Strict qualified as Map
import Data.String.Interpolate
import Effectful
import Effectful.Error.Static
import Effectful.Reader.Static
import Error.Diagnose (Diagnostic, diagnosticToJson)
import Niveo.Interpreter (Context (..), Val, evalTxt, throwReport)
import Niveo.Interpreter.FileSystem (FSMap, FsError (..), runFileSystemPure)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase)
import Tests.Common
import Prelude hiding (runReader)

evalTxtTest :: FSMap -> Text -> Either Text Text
evalTxtTest fs src = runPureEff evalTxtPure & bimap showErr show
  where
    evalTxtPure :: Eff '[] (Either (Diagnostic Text) Val) =
      evalTxt
        & runFileSystemPure fs
        & runErrorNoCallStack @FsError
        >>= either (\(FsError e) -> throwReport e []) pure
        & runErrorNoCallStack @(Diagnostic _)
        & runReader ctx
    ctx = Context {env = def, fin = "<test>", src}
    showErr = decodeUtf8 . diagnosticToJson

assertEvalFS, assertEvalErrorFS :: FSMap -> Text -> Text -> Assertion
assertEvalFS fs = assert' . evalTxtTest fs
assertEvalErrorFS fs = assertError' . evalTxtTest fs

assertEval, assertEvalError :: Text -> Text -> Assertion
assertEval = assertEvalFS def
assertEvalError = assertEvalErrorFS def

test_arithmetic :: TestTree
test_arithmetic =
  testGroup
    "Should evaluate unary expressions"
    [ testCase "with precedence" $
        "+2 / (9 -6 + 1  )- 3 *-5.7 ** 1 ** -6" `assertEval` "17.6",
      testCase "with string concat" $
        [i|"Hello," + " world!"|] `assertEval` [i|"Hello, world!"|],
      testCase "with simple `let`" $
        "let a = 9; a * 6" `assertEval` "54",
      testCase "with simple `let`, undefined variable" $
        "let a = 9; b * 6" `assertEvalError` [i|`b` not found in this scope|],
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
          `assertEval` "42",
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
          `assertEval` "10086"
    ]

test_coll :: TestTree
test_coll =
  testGroup
    "Should evaluate collection literals and operations"
    [ testCase "with list concat" $
        "['what, 'is] + ['love]" `assertEval` "['what, 'is, 'love]",
      testCase "with list indexing" $
        "[0, 1, 2, 3][3]" `assertEval` "3",
      testCase "with list indexing, invalid index" $
        "[0, 1, 2, 3][4]" `assertEvalError` "index out of bounds",
      testCase "with struct, no trailing comma" $
        [i|struct{"foo" + "bar" = 4.2, "ba z" = true && false}|]
          `assertEval` [i|struct{"foobar" = 4.2, "ba z" = false}|],
      testCase "with struct, trailing comma, key shorthand, list" $
        [__i|
          let foo = '__some_foo__;
          let bar = 42;
          struct{
            foo,
            'bar,
            baz: [1, 2.3],
          }
        |]
          `assertEval` [i|struct{"foo" = '__some_foo__, 'bar = 42, "baz" = [1, 2.3]}|],
      testCase "with struct accessors" $
        [__i|
          let s = struct{one: "Night", 'blinding = "Lights"};
          s.one + s::blinding
        |]
          `assertEval` [i|"NightLights"|],
      testCase "with struct accessors, invalid entry" $
        [i|struct{one: "Night"}.two|] `assertEvalError` "no entry found"
    ]

test_bool :: TestTree
test_bool =
  testGroup
    "Should parse booleans"
    [ testCase "with comparisons" $
        "  -(-1+2) >=3 ==   ! !(4 <5+ (6/ 7))   " `assertEval` "false",
      testCase "with `&&`, `||` and `!`" $
        [__i|
          let foo = 'foo, bar = true, a = !bar, b = 3;
          foo == null || !!bar && a != (b == 3)
        |]
          `assertEval` "true",
      testCase "with `&&` and `||`, short-circuiting" $
        [__i|
          let should_be_true = 2 + 2 == 4 || whatever;
          let should_be_false = 2 * 2 != 4 && null;
          should_be_true && !should_be_false
        |]
          `assertEval` "true"
    ]

test_call :: TestTree
test_call =
  testGroup
    "Should evaluate function calls"
    [ testCase "with simple call" $
        "let abs = fun(a) {if (a >= 0) a else -a}; abs(-4) * 10 + abs(2)" `assertEval` "42",
      testCase "with inline call, captures" $
        "let one = 1; fun(a) {a + one}(41)" `assertEval` "42",
      testCase "with complex call" $
        "let add_c = fun(a) {fun(b) {a + b}}; add_c(40)(2)" `assertEval` "42",
      testCase "with recursive definition" $
        "letrec fact = fun(a) {if (a <= 1) 1 else a * fact(a-1)}; fact(5)" `assertEval` "120",
      testCase "with mutual recursive definitions" $
        [__i|
          let n = 10;
          letrec
            even = fun(x) {if (x == 0) true else odd(x-1)},
            odd = fun(x) {if (x == 0) false else even(x-1)};
          even(n)
        |]
          `assertEval` "true"
    ]

test_import :: TestTree
test_import =
  testGroup
    "Should parse booleans"
    [ testCase "with simple import" $
        [i|import("three.niv") + 39|] `assertEval'` "42",
      testCase "with invalid import" $
        [i|import("four.niv") + 38|] `assertEvalError'` "file `four.niv` not found",
      testCase "with nested import" $
        [i|let s = import("struct.niv"); s.three * s::four|] `assertEval'` "12"
    ]
  where
    assertEval' = assertEvalFS fs
    assertEvalError' = assertEvalErrorFS fs
    fs =
      Map.fromList
        [ ("three.niv", "3"),
          ( "struct.niv",
            [__i|
              let three = import("three.niv");
              let incr = fun(x) { x+1 };
              let four = incr(three);
              struct {
                three,
                'four,
              }
            |]
          )
        ]
