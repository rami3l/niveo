module Tests.Parser where

import Data.String.Interpolate
import Niveo.Parser (Parser, expression, program)
import Relude
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))
import Tests.Common (assertRegexMatch)
import Text.Megaparsec (errorBundlePretty, parse)
import Text.RawString.QQ

parse' :: Show a => Parser a -> Text -> Either Text Text
parse' parser got = parse parser "" got & bimap (toText . errorBundlePretty) show

assert' :: (Show a, Eq a) => Either Text a -> a -> Assertion
assert' (Right got) expected = got @?= expected
assert' (Left e) _ = error [i|test failed: got `#{e}`|]

assertError :: Show a => Either Text a -> Text -> Assertion
assertError (Right a) _ = putStrLn $ "test did not fail as expected: got `" <> show a <> "`"
assertError (Left got) pattern' = got `assertRegexMatch` pattern'

-- Expressions:

assertExpr, assertExprError :: Text -> Text -> Assertion
assertExpr = assert' . parse' expression
assertExprError = assertError . parse' expression

test_arithmetic :: TestTree
test_arithmetic =
  testGroup
    "Should parse arithmetic expressions"
    [ testCase "with precedence" $
        "1+2 / 3- 4 *-5.678" `assertExpr` "(- (+ 1 (/ 2 3)) (* 4 (- 5.678)))",
      testCase "with missing unary operand" $
        "-" `assertExprError` "expecting operand$",
      testCase "with missing binary operand" $
        "1-" `assertExprError` "expecting operand$",
      testCase "with parens" $
        "-(-1+2 / 3- 4 *5+ (6/ 7))"
          `assertExpr` "(- (+ (- (+ (- 1) (/ 2 3)) (* 4 5)) (/ 6 7)))",
      testCase "with paren mismatch" $
        "-(-1+2 / 3- 4 *5+ (6/ 7)" `assertExprError` [r|expecting '\)'$|],
      testCase "with binary misused as unary" $
        "*1" `assertExprError` "expecting expression$",
      testCase "with assignments" $
        "a = b = c = 3" `assertExpr` "(assign! a (assign! b (assign! c 3)))"
    ]

test_boolean :: TestTree
test_boolean =
  testGroup
    "Should parse booleans"
    [ testCase "with `>=`" $
        "-(-1+2) >=3- 4 *5+ (6/ 7)"
          `assertExpr` "(>= (- (+ (- 1) 2)) (+ (- 3 (* 4 5)) (/ 6 7)))",
      testCase "with `>=`, misused as unary" $
        ">= 1+2 == 3"
          `assertExprError` "expecting expression$",
      testCase "with `and`, `or` and `!`" $
        "foo == nil or !!bar and a != (b = c = 3)"
          `assertExpr` "(or (== foo nil) (and (! (! bar)) (!= a (assign! b (assign! c 3)))))"
    ]

test_call :: TestTree
test_call =
  testGroup
    "Should parse function calls and get/set expressions"
    [ testCase "with complex call" $
        "func (c) (u, r) (r(y), i) (n) (g) ()"
          `assertExpr` "((((((func c) u r) (r y) i) n) g))",
      testCase "with complex call, typo" $
        "func (c) (u, r (r(y), i) (n) (g) ()"
          `assertExprError` [r|expecting '\)'$|],
      testCase "with gets and sets" $
        "breakfast.omelette.filling.meat = ham"
          `assertExpr` "(.set! (. (. breakfast omelette) filling) meat ham)",
      testCase "with chained method calls" $
        "egg.scramble(3).with(cheddar)"
          `assertExpr` "((. ((. egg scramble) 3) with) cheddar)",
      testCase "with nested method calls" $
        "he.breakfast(omelette.filledWith(cheese), sausage)"
          `assertExpr` "((. he breakfast) ((. omelette filledWith) cheese) sausage)",
      testCase "with `super`" $ "super.method()" `assertExpr` "((. (super) method))"
    ]

-- Statements (and Declarations):

assertProg :: Text -> [Text] -> Assertion
assertProg = assert' . bimap (toText . errorBundlePretty) (show <$>) . parse program ""

assertProgError :: Text -> Text -> Assertion
assertProgError = assertError . parse' program

test_simple :: TestTree
test_simple =
  testGroup
    "Should parse simple statements"
    [ testCase "with `print`" $ "print foo;" `assertProg` ["(print foo)"],
      testCase "with `print`, complex expression" $
        "print -(-1+2) >=3;"
          `assertProg` ["(print (>= (- (+ (- 1) 2)) 3))"],
      testCase "with expression" $ "foo;" `assertProg` ["foo"],
      testCase "with expression, missing semicolon" $
        "foo" `assertProgError` "expecting ';'$",
      testCase "with `var`" $ "var foo;" `assertProg` ["(var foo)"],
      testCase "with `var`, init'd" $
        "var foo = 42;" `assertProg` ["(var foo 42)"],
      testCase "with block" $
        "var foo; { var bar = 1; print bar; } var baz;"
          `assertProg` ["(var foo)", "(begin (var bar 1) (print bar))", "(var baz)"],
      testCase "with block, typo" $
        "var foo; { var bar = 1; print bar; var baz;"
          `assertProgError` "expecting '}'$"
    ]

test_controlFlow :: TestTree
test_controlFlow =
  testGroup
    "Should parse control flows"
    [ testCase "with `if`-`else`" $
        "var year; if (2 + 2 == 5) year = 1984; else year = 2022;"
          `assertProg` [ "(var year)",
                         "(if (== (+ 2 2) 5) (assign! year 1984) (assign! year 2022))"
                       ],
      testCase "with `if`, missing `then` branch" $
        "var year; if (2 + 2 == 5)" `assertProgError` "expecting then branch$",
      testCase "with `if`" $
        "var year; if (2 + 2 == 5) year = 1984;"
          `assertProg` [ "(var year)",
                         "(if (== (+ 2 2) 5) (assign! year 1984))"
                       ],
      testCase "with `if`, nested & unbraced" $
        [__i|
          if (first)
            if (second) whenTrue;
            else whenFalse;
        |]
          `assertProg` ["(if first (if second whenTrue whenFalse))"],
      testCase "with `while`" $
        "while (i <= 5) { product = product * i; i = i + 1; }"
          `assertProg` ["(while (<= i 5) (begin (assign! product (* product i)) (assign! i (+ i 1))))"],
      testCase "with `while`, jumps" $
        [__i|
          while (true) {
            if (i == 3 or i == 5) {
              i = i + 1;
              continue;
            }
            product = product * i;
            i = i + 1;
            if (i > 6) break;
          }
        |]
          `assertProg` ["(while true (begin (if (or (== i 3) (== i 5)) (begin (assign! i (+ i 1)) (continue))) (assign! product (* product i)) (assign! i (+ i 1)) (if (> i 6) (break))))"],
      testCase "with `for`" $
        "for (i = product = 1; i <= 5; i = i + 1) { product = product * i; }"
          `assertProg` ["(begin (assign! i (assign! product 1)) (while (<= i 5) (begin (begin (assign! product (* product i))) (assign! i (+ i 1)))))"],
      testCase "with `for`, pure loop" $
        "for (;;) { product = product * i; }"
          `assertProg` ["(begin (while true (begin (begin (assign! product (* product i))))))"],
      testCase "with `for`, typo" $
        "for (i = product = 1; i <= 5, i = i + 1) { product = product * i; }"
          `assertProgError` "expecting ';'$"
    ]

test_decl :: TestTree
test_decl =
  testGroup
    "Should parse declarations"
    [ testCase "with `fun`, noop" $ "fun foo() { }" `assertProg` ["(fun foo () '())"],
      testCase "with `fun`, body" $
        "fun foo_bar() { print 1 * 2 - 3 / 4; }"
          `assertProg` ["(fun foo_bar () (print (- (* 1 2) (/ 3 4))))"],
      testCase "with `fun`, params & body" $
        "fun foo_bar(a, b, c, d) { print a * b - c / d; }"
          `assertProg` ["(fun foo_bar (a b c d) (print (- (* a b) (/ c d))))"],
      testCase "with `class`" $
        [r|class Foo { bar(baz, boo) { return this + ": Boom"; } }|]
          `assertProg` [[r|(class Foo ((fun bar (baz boo) (return (+ (this) ": Boom")))))|]],
      testCase "with `class`, superclass" $
        [r|class Foo < Bar { bar(baz, boo) { return this + ": Boom"; } }|]
          `assertProg` [[r|(class Foo (<: Bar) ((fun bar (baz boo) (return (+ (this) ": Boom")))))|]],
      testCase "with `class`, no class name" $
        "class" `assertProgError` "expecting class name$",
      testCase "with `class`, no superclass" $
        "class Foo <" `assertProgError` "expecting superclass$",
      testCase "with `class`, no body" $
        "class Foo" `assertProgError` [r|expecting '\{'$|]
    ]

test_sync :: TestTree
test_sync =
  testGroup
    "Should report multiple parsing errors"
    [ testCase "with `if` and `class`" $
        ("if (true) class A {} var a = 4; 5" `assertProgError`)
          `mapM_` [ "keyword `class` cannot be an identifier$",
                    "expecting ';'$"
                  ],
      testCase "with `else` and `var`" $
        ("print else var = 5" `assertProgError`)
          `mapM_` [ "keyword `else` cannot be an identifier$",
                    "expecting identifier$"
                  ]
    ]

-- Extra:

test_extra :: TestTree
test_extra =
  testGroup
    "Should parse extra syntax"
    [exprs, stmts]
  where
    exprs =
      testGroup
        "expressions"
        [ testCase "with lambda, noop" $ "fun () { }" `assertExpr` "(lambda () '())",
          testCase "with lambda, in-place call" $
            "fun () { } ()"
              `assertExpr` "((lambda () '()))",
          testCase "with lambda, params & body" $
            "fun (a, b, c, d) { print a * b - c / d; }"
              `assertExpr` "(lambda (a b c d) (print (- (* a b) (/ c d))))"
        ]
    stmts =
      testGroup
        "statements"
        [ testCase "with lambda, statement" $ "(fun () {});" `assertProg` ["(lambda () '())"],
          testCase "with lambda, param" $ "g(fun () {});" `assertProg` ["(g (lambda () '()))"]
        ]
