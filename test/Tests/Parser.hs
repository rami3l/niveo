module Tests.Parser where

import Data.String.Interpolate
import Niveo.Parser (Parser, expression)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase)
import Tests.Common
import Text.Megaparsec (MonadParsec (eof), errorBundlePretty, parse)

parse' :: Show a => Parser a -> Text -> Either Text Text
parse' parser got = parse parser "" got & bimap (toText . errorBundlePretty) show

assertExpr, assertExprError :: Text -> Text -> Assertion
assertExpr = assert' . parse' (expression <* eof)
assertExprError = assertError' . parse' (expression <* eof)

test_arithmetic :: TestTree
test_arithmetic =
  testGroup
    "Should parse arithmetic expressions"
    [ testCase "with precedence" $
        "1+2 / 3- 4 *-5.6 ** 7 ** 8" `assertExpr` "(- (+ 1 (/ 2 3)) (* 4 (** (- 5.6) (** 7 8))))",
      testCase "with missing unary operand" $
        "-" `assertExprError` "expecting operand$",
      testCase "with missing binary operand" $
        "1-" `assertExprError` "expecting operand$",
      testCase "with parens" $
        "-(-1+2 / 3- 4 *5+ (6/ 7))"
          `assertExpr` "(- (+ (- (+ (- 1) (/ 2 3)) (* 4 5)) (/ 6 7)))",
      testCase "with paren mismatch" $
        "-(-1+2 / 3- 4 *5+ (6/ 7)" `assertExprError` [i|expecting '\\)'$|],
      testCase "with binary misused as unary" $
        "*1" `assertExprError` "expecting expression$",
      testCase "with nested `let` and `letrec`" $
        [__i|
          let a =
            let b = c + { let d = e; d + 1 };
            b;
          letrec f = a + g;
          f
        |]
          `assertExpr` "(let ((a (let ((b (+ c (let ((d e)) (+ d 1))))) b))) (letrec ((f (+ a g))) f))",
      testCase "with `if-else`, `let`, blocks" $
        [__i|
          a + {
            let b = if (p) {
              c || d
            } else if (q) {
              e
            } else {
              f
            };
            g && b
          }
        |]
          `assertExpr` "(+ a (let ((b (if p (|| c d) (if q e f)))) (&& g b)))"
    ]

test_str :: TestTree
test_str =
  testGroup
    "Should parse string literals"
    [ testCase "simple" $ [i|"W O W"|] `assertExpr` [i|"W O W"|],
      testCase "multiline" $
        [__i|"
            Is this the real life?
            Is this just fantasy?
          "|]
          `assertExpr` [i|"\\n  Is this the real life?\\n  Is this just fantasy?\\n"|]
    ]

test_coll :: TestTree
test_coll =
  testGroup
    "Should parse collection literals"
    [ testCase "with struct, no trailing comma" $
        [i|struct{"foo" + "bar" = 4.2, "ba z" = true && false}|]
          `assertExpr` [i|(struct (((+ "foo" "bar") 4.2) ("ba z" (&& true false))))|],
      testCase "with struct, variable as key, no trailing comma" $
        [i|struct{f = g}|]
          `assertExpr` [i|(struct ((f g)))|],
      testCase "with struct, trailing comma, key shorthand, list" $
        [__i|
          struct{
            foo,
            'bar,
            baz: [1, 2.3, "4.56"], // <- A list!
          }
        |]
          `assertExpr` [i|(struct (("foo" foo) ('bar bar) ("baz" (list 1 2.3 "4.56"))))|]
    ]

test_fun :: TestTree
test_fun =
  testGroup
    "Should parse lambdas"
    [ testCase "without params" $
        [__i|
          fun() { // Closure lambda
            if (a >= 0) a else -a // Expression-based return
          }
        |]
          `assertExpr` "(lambda '() (if (>= a 0) a (- a)))",
      testCase "with params" $
        "fun(a, b,) { let res = a + b ** a; res }"
          `assertExpr` "(lambda (a b) (let ((res (+ a (** b a)))) res))"
    ]

test_bool :: TestTree
test_bool =
  testGroup
    "Should parse booleans"
    [ testCase "with `>=`" $
        "-(-1+2) >=3- 4 *5+ (6/ 7)"
          `assertExpr` "(>= (- (+ (- 1) 2)) (+ (- 3 (* 4 5)) (/ 6 7)))",
      testCase "with `>=`, misused as unary" $
        ">= 1+2 == 3"
          `assertExprError` "expecting expression$",
      testCase "with `&&`, `||` and `!`" $
        "foo == null || !!bar && a != (b == 3)"
          `assertExpr` "(|| (== foo null) (&& (! (! bar)) (!= a (== b 3))))"
    ]

test_call :: TestTree
test_call =
  testGroup
    "Should parse function calls and index/get expressions"
    [ testCase "with complex call" $
        "f (c) (u, r) (r(y), i,) (n,) (g) ()"
          `assertExpr` "((((((f c) u r) (r y) i) n) g))",
      testCase "with complex call, typo" $
        "f(c) (u, r (r(y), i) (n) (g) ()"
          `assertExprError` [i|expecting '\\)'$|],
      testCase "with indices and gets" $
        [i|breakfast["omelette"].filling[40+2]::wow|]
          `assertExpr` [i|(@ (@ (@ (@ breakfast "omelette") "filling") (+ 40 2)) 'wow)|],
      testCase "with chained method calls" $
        "struct{egg: 42}.scramble(3).with(cheddar)"
          `assertExpr` [i|((@ ((@ (struct (("egg" 42))) "scramble") 3) "with") cheddar)|],
      testCase "with nested method calls" $
        "he.breakfast(omelette.filledWith('__cheese__), sausage)"
          `assertExpr` [i|((@ he "breakfast") ((@ omelette "filledWith") '__cheese__) sausage)|],
      testCase "with `let` and lambda, no params" do
        "let f = g(); f()" `assertExpr` [i|(let ((f (g))) (f))|]
        "letrec f = fun() {f()}; f()" `assertExpr` [i|(letrec ((f (lambda '() (f)))) (f))|],
      testCase "with pipes" do
        "a || b |> g.f()" `assertExpr` [i|((@ g "f") (|| a b))|]
        "a || b |> g.f" `assertExpr` [i|((@ g "f") (|| a b))|]
        "a || b |> g.f(42)" `assertExpr` [i|((@ g "f") (|| a b) 42)|]
        "1 |> fun(a) {a + 1}(2,3) |> (g |> h(3.5))(what[4])"
          `assertExpr` "((h g 3.5) ((lambda (a) (+ a 1)) 1 2 3) (@ what 4))"
    ]
