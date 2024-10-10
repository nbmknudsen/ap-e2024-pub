module APL.Interp_Tests (tests) where

import APL.AST (Exp (..))
import APL.Eval (eval)
import APL.InterpIO (runEvalIO)
import APL.InterpPure (runEval)
import APL.Monad
import APL.Util
import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import System.IO
import System.Process (createPipe)
import Test.Tasty (TestTree, localOption, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.Runners (NumThreads (..))

eval' :: Exp -> ([String], Either Error Val)
eval' = runEval . eval

evalIO' :: Exp -> IO (Either Error Val)
evalIO' = runEvalIO . eval

tests :: TestTree
tests = testGroup "Free monad interpreters" [pureTests, ioTests']
  where
    ioTests' = localOption (NumThreads 1) ioTests

pureTests :: TestTree
pureTests =
  testGroup
    "Pure interpreter"
    [ testCase "Pure CstInt" $
        eval' (CstInt 7)
          @?= ([], Right $ ValInt 7),
      --
      testCase "Let" $
        eval' (Let "x" (Add (CstInt 2) (CstInt 3)) (Var "x"))
          @?= ([], Right $ ValInt 5),
      --
      testCase "localEnv" $
        runEval
          ( localEnv (const [("x", ValInt 1)]) $
              askEnv
          )
          @?= ([], Right $ [("x", ValInt 1)]),
      --
      testCase "Let (shadowing)" $
        eval'
          ( Let
              "x"
              (Add (CstInt 2) (CstInt 3))
              (Let "x" (CstBool True) (Var "x"))
          )
          @?= ([], Right $ ValBool True),
      --
      testCase "State" $
        runEval
          ( do
              putState [(ValInt 0, ValInt 1)]
              modifyState $ map (\(key, _) -> (key, ValInt 5))
              getState
          )
          @?= ([], Right $ [(ValInt 0, ValInt 5)]),
          --
          testCase "Print" $
            runEval (evalPrint "test")
              @?= (["test"], Right ()),
          --
          testCase "Error" $
            runEval
              ( do
                  _ <- failure "Oh no!"
                  evalPrint "test"
              )
              @?= ([], Left "Oh no!")
      ]

ioTests :: TestTree
ioTests =
  testGroup
    "IO interpreter"
    [ testCase "print" $ do
     let s1 = "Lalalalala"
         s2 = "Weeeeeeeee"
     (out, res) <-
       captureIO [] $
         runEvalIO $ do
           evalPrint s1
           evalPrint s2
     (out, res) @?= ([s1, s2], Right ()),
     --
     testCase "print" $ do
        (out, res) <-
          testIO [] $
            evalIO' $
              Print "This is also 1" $
                Print "This is 1" $
                  CstInt 1
        (out, res) @?= (["This is 1: 1", "This is also 1: 1"], Right $ ValInt 1)
    ]

-- DO NOT MODIFY
testIO :: [String] -> IO a -> IO ([String], a)
testIO inputs m = do
  hFlush stdout
  threadDelay 10000 -- Needed to make sure things are actually flushed
  stdin' <- hDuplicate stdin
  stdout' <- hDuplicate stdout

  (inR, inW) <- createPipe
  (outR, outW) <- createPipe

  hSetBuffering inW NoBuffering
  hSetBuffering outW NoBuffering

  bracket
    ( do
        inR `hDuplicateTo` stdin
        outW `hDuplicateTo` stdout
    )
    ( \_ -> do
        stdin' `hDuplicateTo` stdin
        stdout' `hDuplicateTo` stdout
        mapM_ hClose [stdin', stdout', inR, inW, outW]
    )
    ( \_ -> do
        mapM_ (hPutStrLn inW) inputs
        hFlush inW

        res <- m

        output <- hGetContents outR -- hGetContents closes outR
        pure (lines output, res)
    )
