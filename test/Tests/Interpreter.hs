module Tests.Interpreter where

import Data.Aeson qualified as Aeson
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
import Test.Tasty.QuickCheck (testProperty)
import Tests.Common
import Witch
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
      testCase "with `let`" do
        "let a = 9; a * 6" `assertEval` "54"
        "let a = 9; b * 6" `assertEvalError` [i|`b` not found in this scope|]
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
        "['what,'is] + ['love]" `assertEval` "['what, 'is, 'love]",
      testCase "with string concat" $
        [i|"what is " + "love"|] `assertEval` [i|"what is love"|],
      testCase "with list indexing" do
        "range(0, 4)[3]" `assertEval` "3"
        "range(0, 4)[[1,3]]" `assertEval` "[1, 3]"
        "range(0, 4)[-1]" `assertEvalError` "index out of bounds"
        "range(0, 4)[4]" `assertEvalError` "index out of bounds"
        "range(0, 4)[[1,4]]" `assertEvalError` "index out of bounds",
      testCase "with string indexing" do
        [i|"abcd"[3]|] `assertEval` [i|"d"|]
        [i|"abcd"[[1,3]]|] `assertEval` [i|"bd"|]
        [i|"abcd"[-1]|] `assertEvalError` "index out of bounds"
        [i|"abcd"[4]|] `assertEvalError` "index out of bounds"
        [i|"abcd"[[1,4]]|] `assertEvalError` "index out of bounds",
      testCase "with struct" do
        [i|struct{"foo" + "bar" = 4.2, "ba z" = true && false}|]
          `assertEval` [i|struct{"foobar" = 4.2, "ba z" = false}|]
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
      testCase "with struct accessors" do
        [__i|
          let s = struct{one: "Night", 'blinding = "Lights"};
          s.one + s::blinding
        |]
          `assertEval` [i|"NightLights"|]
        [i|struct{one: "Night"}.two|] `assertEvalError` "no entry found",
      testCase "with prelude function `get`" do
        let l = "[0, 1, 22, 333]"
        (l <> "|> get(2)") `assertEval` "22"
        (l <> "|> get(22)") `assertEval` "null"
        (l <> "|> get([2,3])") `assertEval` "[22, 333]"
        (l <> "|> get([2,3,-1])") `assertEval` "null"
        let s = [i|"abcd"|]
        (s <> "|> get(2)") `assertEval` [i|"c"|]
        (s <> "|> get(22)") `assertEval` "null"
        (s <> "|> get([2,3])") `assertEval` [i|"cd"|]
        (s <> "|> get([2,3,-1])") `assertEval` "null"
        let st = "struct{'foo = 40, 'bar = 42, 'foo = 4}"
        (st <> "|> get('bar)") `assertEval` "42"
        (st <> [i||> get("bar")|]) `assertEval` "null"
        (st <> "|> get(['baz, 'foo])") `assertEval` "[null, 40]",
      testCase "with prelude function `prepend`" $
        "struct{'foo = 40, 'bar = 42} |> prepend('bar, 10086)"
          `assertEval` "struct{'bar = 10086, 'foo = 40, 'bar = 42}",
      testCase "with prelude function `delete`" $
        "struct{'bar = 10086, 'foo = 40, 'bar = 42} |> delete('bar)"
          `assertEval` "struct{'foo = 40, 'bar = 42}",
      testCase "with prelude function `rename`" $
        [i|struct{'bar = 10086, 'foo = 40, 'bar = 42} |> rename('foo, "baz")|]
          `assertEval` [i|struct{'bar = 10086, "baz" = 40, 'bar = 42}|],
      testCase "with prelude function `update`" $
        "struct{'foo = 40, 'bar = 42} |> update('bar, 10086)"
          `assertEval` "struct{'foo = 40, 'bar = 10086}"
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
    "Should handle imports & exports properly"
    [ testCase "with simple import" do
        [i|import("three.niv") + 39|] `assertEval'` "42"
        [i|import("../../four.niv") + 38|] `assertEvalError'` "file `../../four.niv` not found",
      testCase "with nested import" $
        [i|let s = import("foo/struct.niv"); to_string(s.three, ", ", s::four)|] `assertEval'` [i|"3, 4"|],
      testCase "with JSON import" do
        [i|let s = import_json("joe.json"); s.age|] `assertEval'` "12"
        [i|import_json("foo/struct.niv")|] `assertEvalError'` "`foo/struct.niv` doesn't seem to be a valid JSON file",
      testProperty "can loselessly convert `Aeson.Value` to `Val` and back" $
        \(v :: Aeson.Value) -> v & into @Val & tryInto @Aeson.Value & either (const False) (== v)
    ]
  where
    assertEval' = assertEvalFS fs
    assertEvalError' = assertEvalErrorFS fs
    fs =
      Map.fromList
        [ ("three.niv", "3"),
          ( "foo/struct.niv",
            [__i|
              let three = import("./bar/../../three.niv");
              let incr = fun(x) { x+1 };
              let four = incr(three);
              struct {
                three,
                'four,
              }
            |]
          ),
          ("joe.json", [i|{ "name": "Joe", "age": 12 }|])
        ]

test_std :: TestTree
test_std =
  testGroup
    "Should work with `std` modules"
    [ testCase "with `std/list`" do
        let imp = [i|let list = import("std/list");|]
        (imp <> "range(0,4) |> list::map(fun(a){a ** 2})")
          `assertEval` "[0, 1, 4, 9]"
        (imp <> "range(0,4) |> list::filter(fun(a){mod(a, 2) == 0})")
          `assertEval` "[0, 2]"
        (imp <> "range(0,4) |> list::foldl([], fun(acc, it){acc + [it]})")
          `assertEval` "[0, 1, 2, 3]"
        (imp <> "range(0,4) |> list::foldr([], fun(acc, it){acc + [it]})")
          `assertEval` "[3, 2, 1, 0]"
    ]
