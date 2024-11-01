module SPC_Tests (tests) where

import Control.Concurrent (threadDelay)
import SPC
import Test.Tasty (TestTree, localOption, mkTimeout, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  localOption (mkTimeout 3000000) $
    testGroup
      "SPC (core)"
      [ 
        testCase "addWorker" $ do
          spc <- startSPC
          -- Attempt to add a new worker
          result <- workerAdd spc "worker1"
          case result of
            Right _ -> pure ()
            Left err -> assertFailure $ "Expected successful worker addition, but got error: " ++ err

          -- Attempt to add the same worker again, should fail
          result2 <- workerAdd spc "worker1"
          case result2 of
            Left err -> err @?= "Worker name already exists"
            Right _ -> assertFailure "Expected worker addition to fail, but it succeeded."

          -- Adding a second worker with a different name should succeed
          result3 <- workerAdd spc "worker2"
          case result3 of
            Right _ -> pure ()
            Left err -> assertFailure $ "Expected successful worker addition, but got error: " ++ err
        ,  
        
        testCase "canceling job" $ do
          spc <- startSPC
          j <- jobAdd spc $ Job (pure ()) 1
          jobCancel spc j
          r <- jobStatus spc j
          r @?= (JobDone DoneCancelled)
        ,
        
        testCase "canceling job makes worker idle" $ do
          spc <- startSPC
          Right _worker <- workerAdd spc "worker1"
          let longRunningJob = Job (threadDelay 2000000) 1  -- 2 seconds job
          j <- jobAdd spc longRunningJob
          jobCancel spc j
          threadDelay 1000000
          r <- jobStatus spc j
          r @?= JobDone DoneCancelled

          
          let longRunningJob2 = Job (threadDelay 1000) 1  -- 2 seconds job
          j10 <- jobAdd spc longRunningJob2
          r11 <- jobStatus spc j10
          r11 @?= JobRunning
          threadDelay 1000000
          r12 <- jobStatus spc j10
          r12 @?= JobDone Done
          ,
        
        testCase "timeout" $ do
          spc <- startSPC
          Right _worker <- workerAdd spc "worker1"
          let longRunningJob = Job { jobAction = threadDelay 2000000
                                    , jobMaxSeconds = 0
                                    }
          j <- jobAdd spc longRunningJob
          r1 <- jobStatus spc j
          r1 @?= JobRunning
          threadDelay 1000000
          r2 <- jobStatus spc j
          r2 @?= JobDone DoneTimeout
          Left failWorker <- workerAdd spc "worker1"
          failWorker @?= "Worker name already exists"

          let longRunningJob2 = Job (threadDelay 1000) 1  -- 2 seconds job
          j10 <- jobAdd spc longRunningJob2
          r11 <- jobStatus spc j10
          r11 @?= JobRunning
          threadDelay 1000000
          r12 <- jobStatus spc j10
          r12 @?= JobDone Done
          ,
        
        testCase "remove worker stopped" $ do
          spc <- startSPC
          Right worker <- workerAdd spc "worker1"
          workerStop worker
          threadDelay 2000000
          newWorker <- workerAdd spc "worker1"
          case newWorker of
            Right _ -> pure ()
            Left err -> assertFailure $ "Expected successful worker addition, but got error: " ++ err
          ,

        testCase "remove worker busy" $ do
          spc <- startSPC
          let longRunningJob = Job (threadDelay 2000000) 1  -- 2 seconds job
          j <- jobAdd spc longRunningJob
          r1 <- jobStatus spc j
          r1 @?= JobPending
          Right worker <- workerAdd spc "worker1"
          r3 <- jobStatus spc j
          r3 @?= JobRunning
          workerStop worker
          threadDelay 1000000
          r2 <- jobStatus spc j
          r2 @?= JobDone DoneCancelled
          result <- workerAdd spc "worker1"
          case result of
            Right _ -> pure ()
            Left err -> assertFailure $ "Expected successful worker addition, but got error: " ++ err
         ,

        testCase "Job raises an exception" $ do
          spc <- startSPC
          Right _ <- workerAdd spc "worker1"

          let job1 = Job { jobAction = error "Intentional Exception"
                         , jobMaxSeconds = 1
                         }
          jobId1 <- jobAdd spc job1
          threadDelay 1000000
          reason1 <- jobStatus spc jobId1
          reason1 @?= JobDone DoneCrashed

          let longRunningJob2 = Job (threadDelay 1000) 1  -- 2 seconds job
          j10 <- jobAdd spc longRunningJob2
          r11 <- jobStatus spc j10
          r11 @?= JobRunning
          threadDelay 1000000
          r12 <- jobStatus spc j10
          r12 @?= JobDone Done
      ]