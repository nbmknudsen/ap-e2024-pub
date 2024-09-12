module APL.Eval_Tests (tests) where

import APL.AST (Exp(..))
import APL.Eval (Val(..), eval, envEmpty)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Evaluation"
    [
      testCase "Evaluate constant integer" $
        eval envEmpty (CstInt 42) @?= Right (ValInt 42), 

      testCase "Evaluate constant negative integer" $
        eval envEmpty (CstInt(-42)) @?= Right (ValInt (-42)), 

      testCase "Evaluate adding" $
        eval envEmpty (Add (CstInt 2) (CstInt 3)) @?= Right (ValInt 5),

      testCase "Evaluate adding with wrong type" $
        eval envEmpty (Add (CstBool True) (CstInt 3)) @?= Left "Wrong type",

      testCase "Evaluate subtraction" $
        eval envEmpty (Sub (CstInt 3) (CstInt 2)) @?= Right (ValInt 1),

      testCase "Evaluate multiplication" $
        eval envEmpty (Mul (CstInt 3) (CstInt 4)) @?= Right (ValInt 12),

      testCase "Evaluate division" $
        eval envEmpty (Div (CstInt 12) (CstInt 3)) @?= Right (ValInt 4),

      testCase "Evaluate division by zero" $
        eval envEmpty (Div (CstInt 12) (CstInt 0)) @?= Left "Division by zero",

      testCase "Evaluate exponentation" $
        eval envEmpty (Pow (CstInt 2) (CstInt 3)) @?= Right (ValInt 8),

      testCase "Evaluate exponentation with negative exponent" $
        eval envEmpty (Pow (CstInt 2) (CstInt (-3))) @?= Left "Negative exponent",

      testCase "Evaluate boolean True" $
        eval envEmpty (CstBool True) @?= Right (ValBool True), 

      testCase "Evaluate boolean False" $
        eval envEmpty (CstBool False) @?= Right (ValBool False),

      testCase "Evaluate equality Integer True" $
        eval envEmpty (Eql (CstInt 2) (CstInt 2)) @?= Right (ValBool True), 

      testCase "Evaluate equality Integer False" $
        eval envEmpty (Eql (CstInt 2) (CstInt 3)) @?= Right (ValBool False),

      testCase "Evaluate equality Boolean True" $
        eval envEmpty (Eql (CstBool True) (CstBool True)) @?= Right (ValBool True), 

      testCase "Evaluate equality Boolean False" $
        eval envEmpty (Eql (CstBool False) (CstBool True)) @?= Right (ValBool False),

      testCase "Evaluate equality with different types" $
        eval envEmpty (Eql (CstBool False) (CstInt 3)) @?= Left "Different types",

      testCase "Evaluate if statement with Integer" $
        eval envEmpty (If (CstInt 2) (CstInt 3) (CstInt 4)) @?= Left "Condition is non-boolean",

      testCase "Evaluate if statement" $
        eval envEmpty (If (Eql (CstInt 2) (CstInt 2)) (CstInt 3) (CstInt 4)) @?= Right (ValInt 3),

      testCase "Evaluate Let statement" $
        eval envEmpty (Let "x" (CstInt 3) (Add (Var "x") (Var "x"))) @?= Right (ValInt 6),

      testCase "Evaluate Let statement with Var" $
      eval envEmpty (Let "x" (CstInt 3) (Add (Var "x") (Var "y"))) @?= Left "Unknown variable: y"

    ]
    
{-,
      ,
      testCase "Evaluate subtraction" $
        eval (Sub (CstInt 3) (CstInt 2)) @?= ValInt 1,
      testCase "Evaluate multiplication" $
        eval (Mul (CstInt 3) (CstInt 4)) @?= ValInt 12,
      testCase "Evaluate division" $
        eval (Div (CstInt 12) (CstInt 3)) @?= ValInt 4, 
      testCase "Evaluate exponentation" $
        eval (Pow (CstInt 2) (CstInt 3)) @?= ValInt 8-}