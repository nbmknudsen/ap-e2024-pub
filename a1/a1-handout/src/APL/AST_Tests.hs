module APL.AST_Tests (tests) where

import APL.AST (Exp (..), printExp,)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Prettyprinting"
    [ testCase "Add" $
        printExp (Add (CstInt 2) (CstInt 5))
          @?= "2 + 5",
      --
      testCase "Sub" $
        printExp (Sub (CstInt 2) (CstInt 5))
          @?= "2 - 5",
      --
      testCase "Mul" $
        printExp (Mul (CstInt 2) (CstInt 5))
          @?= "2 * 5",
      --
      testCase "Div" $
        printExp (Div (CstInt 2) (CstInt 5))
          @?= "2 / 5",
      --
      testCase "Pow" $
        printExp (Pow (CstInt 2) (CstInt 5))
          @?= "2 ** 5",
      --
      testCase "Eql" $
        printExp (Eql (CstInt 2) (CstInt 5))
          @?= "2 == 5",
      --
      testCase "If" $
        printExp (If (Eql (CstInt 2) (CstInt 3)) (Add (CstInt 5) (CstInt 7)) (Sub (CstInt 5) (CstInt 7)))
          @?= "if 2 == 3 then 5 + 7 else 5 - 7",
      --
      testCase "Let" $
        printExp (Let "x" (CstInt 5) (Sub (Var "x") (CstInt 9)))
          @?= "let x = 5 in x - 9",
      --
      testCase "Lambda" $
        printExp (Lambda "x" (Sub (CstInt 5) (CstInt 7)))
          @?= "\\x -> 5 - 7",
      --
      testCase "Apply" $
        printExp(Apply(Let "x" (CstInt 2)(Lambda "y" (Add (Var "x") (Var "y")))) (CstInt 3))
          @?= "let x = 2 in \\y -> x + y 3",
      --
      testCase "TryCatch" $
        printExp (TryCatch (CstInt 0) (CstInt 1))
          @?= "try 0 catch 1",
      --
      testCase "TryCatch missing" $
        printExp (TryCatch (Var "missing") (CstInt 1))
          @?= "try missing catch 1"
    ]
