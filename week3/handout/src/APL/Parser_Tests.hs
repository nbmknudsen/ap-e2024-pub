module APL.Parser_Tests (tests) where

import APL.AST (Exp (..))
import APL.Parser (parseAPL)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

-- Test that some string parses to the provided Exp.
parserTest :: String -> Exp -> TestTree
parserTest s e =
  testCase s $
    case parseAPL "input" s of
      Left err -> assertFailure err
      Right e' -> e' @?= e

-- Test that some string results in a parse error. The exact error
-- message is not tested. Be careful when using this function, as it
-- is easy to make a test that fails for some reason (such as a typo)
-- than the case you are actually interested in. Generally, negative
-- tests of parsers are often not very interesting.
parserTestFail :: String -> TestTree
parserTestFail s =
  testCase s $
    case parseAPL "input" s of
      Left _ -> pure ()
      Right e ->
        assertFailure $
          "Expected parse error but received this AST:\n" ++ show e

tests :: TestTree
tests =
  testGroup
    "Parsing"
    [ testGroup
        "Constants"
        [ parserTest "123" $ CstInt 123,
          parserTest " 123" $ CstInt 123,
          parserTest "123 " $ CstInt 123,
          parserTestFail "123xyz"
        ],
      testGroup
        "Vars"
        [ parserTest "x123" $ Var "x123",
          parserTest " x123" $ Var "x123",
          parserTest "x123 " $ Var "x123",
          parserTestFail "1x123",
          parserTestFail "if"
        ],
      testGroup
        "Booleans"
        [ parserTest "true" $ CstBool True,
          parserTest "false" $ CstBool False,
          parserTestFail "true e",
          parserTest "truee" $ Var "truee"
        ],
      testGroup
        "Operator left asssociative"
        [ parserTest "x+y+z"   $ Add (Add (Var "x") (Var "y")) (Var "z"),
          parserTest "x+y-z"   $ Sub (Add (Var "x") (Var "y")) (Var "z"),
          parserTest "x+y*z"   $ Add (Var "x") (Mul (Var "y") (Var "z")),
          parserTest "x*y*z"   $ Mul (Mul (Var "x") (Var "y")) (Var "z"),
          parserTest "x/y/z"   $ Div (Div (Var "x") (Var "y")) (Var "z"),
          parserTest "x+(y*z)" $ Add (Var "x") (Mul (Var "y") (Var "z"))
        ],
      testGroup
        "Conditional expressions"
        [ parserTest "if x then y else z" $ If (Var "x") (Var "y") (Var "z"),
          parserTest "if x then y else if x then y else z" $
            If (Var "x") (Var "y") $
              If (Var "x") (Var "y") (Var "z"),
          parserTest "if x then (if x then y else z) else z" $
            If (Var "x") (If (Var "x") (Var "y") (Var "z")) (Var "z"),
          parserTest "1 + if x then y else z" $
            Add (CstInt 1) (If (Var "x") (Var "y") (Var "z"))
        ]
    ]
