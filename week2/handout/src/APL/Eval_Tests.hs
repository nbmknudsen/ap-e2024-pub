module APL.Eval_Tests (tests) where

import APL.AST (Exp (..))
import APL.Eval (Error, Val (..), eval, runEval)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

eval' :: Exp -> Either Error Val
eval' = runEval . eval

tests :: TestTree
tests =
  testGroup
    "Evaluation"
    [ testCase "Evaluate constant integer" $
        eval' (CstInt 42) @?= Right (ValInt 42),
      -- 
      testCase "Evaluate constant bool" $
        eval' (CstBool True) @?= Right (ValBool True), 
      --
      testCase "Evaluate Add" $
        eval' (Add (CstInt 2) (CstInt 3)) @?= Right (ValInt 5),
      --
      testCase "Evaluate Add with non-integer" $
        eval' (Add (CstInt 2) (CstBool True)) @?= Left "Non-integer operand",
      --
      testCase "Evaluate Sub" $
        eval' (Sub (CstInt 3) (CstInt 2)) @?= Right (ValInt 1),
      --
      testCase "Evaluate Mul" $
        eval' (Mul (CstInt 3) (CstInt 2)) @?= Right (ValInt 6),
      --
      testCase "Evaluate Div" $
        eval' (Div (CstInt 6) (CstInt 2)) @?= Right (ValInt 3),
      --
      testCase "Evaluate Div with 0" $
        eval' (Div (CstInt 3) (CstInt 0)) @?= Left "Division by zero",
      --
      testCase "Evaluate Pow" $
        eval' (Pow (CstInt 2) (CstInt 3)) @?= Right (ValInt 8),
      --
      testCase "Evaluate Pow with negative exponent" $
        eval' (Pow (CstInt 2) (CstInt (-3))) @?= Left "Negative exponent",
      --
      testCase "Evaluate Eql Integer True" $
        eval' (Eql (CstInt 2) (CstInt 2)) @?= Right (ValBool True), 
      --
      testCase "Evaluate Eql Integer False" $
        eval' (Eql (CstInt 2) (CstInt 3)) @?= Right (ValBool False),
      --
      testCase "Evaluate Eql Boolean True" $
        eval' (Eql (CstBool True) (CstBool True)) @?= Right (ValBool True), 
      --
      testCase "Evaluate Eql Boolean False" $
        eval' (Eql (CstBool False) (CstBool True)) @?= Right (ValBool False),
      --
      testCase "Evaluate Eql with different types" $
        eval' (Eql (CstBool False) (CstInt 3)) @?= Left "Invalid operands to equality",
      --
      testCase "Evaluate If with Integer" $
        eval' (If (CstInt 2) (CstInt 3) (CstInt 4)) @?= Left "Non-boolean condition",
      --
      testCase "Evaluate If" $
        eval' (If (Eql (CstInt 2) (CstInt 2)) (CstInt 3) (CstInt 4)) @?= Right (ValInt 3),
      --
      testCase "Evaluate Let" $
        eval' (Let "x" (CstInt 3) (Add (Var "x") (Var "x"))) @?= Right (ValInt 6),
      --
      testCase "Evaluate Let with Var" $
        eval' (Let "x" (CstInt 3) (Add (Var "x") (Var "y"))) @?= Left "Unknown variable: y",
      --
      testCase "Lambda" $
        eval' (Let "x" (CstInt 2)(Lambda "y" (Add (Var "x") (Var "y"))))
          @?= Right (ValFun [("x", ValInt 2)] "y" (Add (Var "x") (Var "y"))),
      --
      testCase "Apply" $
        eval' (Apply (Let "x" (CstInt 2) (Lambda "y" (Add (Var "x") (Var "y")))) (CstInt 3))
          @?= Right (ValInt 5),
      --
      testCase "Apply non-function" $
        eval' (Apply (Let "x" (CstInt 2) (CstInt 3)) (CstInt 3))
          @?= Left "Cannot apply non-function",
      --
      testCase "TryCatch (first expression succes)" $
        eval' (TryCatch (CstInt 0) (CstInt 1))
          @?= Right (ValInt 0),
      --    
      testCase "TryCatch (first expression error)" $
         eval' (TryCatch (Var "missing") (CstInt 1))
          @?= Right (ValInt 1)
    ]
