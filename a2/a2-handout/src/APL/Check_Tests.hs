module APL.Check_Tests (tests) where

import APL.AST (Exp (..))
import APL.Check (checkExp)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

-- Assert that the provided expression should pass the type checker.
testPos :: Exp -> TestTree
testPos e =
  testCase (show e) $
    checkExp e @?= Nothing

-- Assert that the provided expression should fail the type checker.
testNeg :: Exp -> TestTree
testNeg e =
  testCase (show e) $
    case checkExp e of
      Nothing -> assertFailure "expected error"
      Just _ -> pure ()

tests :: TestTree
tests =
  testGroup
    "Checking"
    [ testPos $ CstInt 2,
      --
      testPos $ CstBool True,
      --
      testPos $ Add (CstInt 2) (CstInt 5),
      --
      testNeg $ Add (CstInt 2) (Var "z"),
      --
      testPos $ Let "z" (CstInt 3) (Add (CstInt 2) (Var "z")),
      --
      testPos $ Sub (CstInt 2) (CstInt 5),
      --
      testNeg $ Sub (CstInt 2) (Var "t"),
      --
      testPos $ Mul (CstInt 2) (CstInt 5),
      --
      testNeg $ Mul (CstInt 2) (Var "z"),
      --
      testPos $ Div (CstInt 4) (CstInt 1),
      --
      testNeg $ Div (CstInt 2) (Var "y"),
      --
      testPos $ Pow (CstInt 4) (CstInt 1),
      --
      testNeg $ Pow (CstInt 2) (Var "x"),
      --
      testPos $ Eql (CstInt 2) (CstInt 3),
      --
      testNeg $ Eql (Var "x") (Var "x"),
      --
      testPos $
        Let "x" (CstInt 2) (If (Eql (Var "x") (CstInt 7) ) (CstInt 2) (Div (CstInt 7) (CstInt 1))),
      --
      testNeg $ If (Eql (Var "x") (CstInt 7) ) (CstInt 2) (Div (CstInt 7) (CstInt 1)),
      --
      testNeg $ If (CstBool True) (Var "x") (Div (CstInt 7) (CstInt 1)),
      --
      testNeg $ If (CstBool True) (CstInt 7) (Var "x"),
      --
      testNeg $ Var "x",
      --
      testPos $ Let "x" (CstInt 3) (Var "x"),
      --
      testNeg $ Let "x" (CstInt 3) (Var "y"),
      --
      testNeg $ Lambda "x" (Var "y"),
      --
      testPos $ Lambda "x" (Var "x"),
      --
      testPos $ Apply (Lambda "x" (Mul (Var "x") (Var "x"))) (CstInt 4),
      --
      testPos $ TryCatch (Div (CstInt 7) (CstInt 0)) (CstBool True),
      --
      testNeg $ TryCatch (Div (CstInt 7) (Var "x")) (CstBool True),
      --
      testPos $ (Print "foo" $ CstInt 2),
      --
      testNeg $ (Print "foo" $ Var "g"),
      --
      testPos $ Let "x" (KvPut (CstInt 0) (CstBool True)) (KvGet (CstInt 0)),
      --
      testPos $ Let "x" (KvPut (CstInt 0) (CstBool True)) (KvGet (CstInt 1))
    ]
