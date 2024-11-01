module SPC
  ( -- * SPC startup
    SPC,
    startSPC,

    -- * Job functions
    Job (..),
    JobId,
    JobStatus (..),
    JobDoneReason (..),
    jobAdd,
    jobStatus,
    jobWait,
    jobCancel,

    -- * Worker functions
    WorkerName,
    workerAdd,
    workerStop,

    -- other
    get,
    spcIdleWorkers,
    runSPCM
  )
where

import Control.Concurrent
  ( forkIO,
    killThread,
    threadDelay,
  )

import Data.List (find)
import Control.Exception (SomeException, catch)
import Control.Monad (ap, forever, liftM, void, forM_, when)
import GenServer
import System.Clock.Seconds (Clock (Monotonic), Seconds, getTime)


-- First some general utility functions.

-- | Retrieve Unix time using a monotonic clock. You cannot use this
-- to measure the actual world time, but you can use it to measure
-- elapsed time.
getSeconds :: IO Seconds
getSeconds = getTime Monotonic

-- | Remove mapping from association list.
removeAssoc :: (Eq k) => k -> [(k, v)] -> [(k, v)]
removeAssoc needle ((k, v) : kvs) =
  if k == needle
    then kvs
    else (k, v) : removeAssoc needle kvs
removeAssoc _ [] = []

-- Then the definition of the glorious SPC.

-- | A job that is to be enqueued in the glorious SPC.
data Job = Job
  { -- | The IO action that comprises the actual action of the job.
    jobAction :: IO (),
    -- | The maximum allowed runtime of the job, counting from when
    -- the job begins executing (not when it is enqueued).
    jobMaxSeconds :: Int
  }

-- | A unique identifier of a job that has been enqueued.
newtype JobId = JobId Int
  deriving (Eq, Ord, Show)

-- | How a job finished.
data JobDoneReason
  = -- | Normal termination.
    Done
  | -- | The job was killed because it ran for too long.
    DoneTimeout
  | -- | The job was explicitly cancelled, or the worker
    -- it was running on was stopped.
    DoneCancelled
  | -- | The job crashed due to an exception.
    DoneCrashed
  deriving (Eq, Ord, Show)

-- | The status of a job.
data JobStatus
  = -- | The job is done and this is why.
    JobDone JobDoneReason
  | -- | The job is still running.
    JobRunning
  | -- | The job is enqueued, but is waiting for an idle worker.
    JobPending
  | -- | A job with this ID is not known to this SPC instance.
    JobUnknown
  deriving (Eq, Ord, Show)

-- | A worker decides its own human-readable name. This is useful for
-- debugging.
type WorkerName = String

-- -- | Messages sent to workers. These are sent both by SPC and by
-- -- processes spawned by the workes.
data WorkerMsg
  = MsgDoJob JobId Job
  | MsgStop
  | MsgCancel JobId
  | MsgCrashed WorkerName JobId
  | MsgJobDone WorkerName JobId
  | MsgTimeout WorkerName JobId

-- Messages sent to SPC.
data SPCMsg
  = -- | Add the job, and reply with the job ID.
    MsgJobAdd Job (ReplyChan JobId)
  | -- | Cancel the given job.
    MsgJobCancel JobId
  | -- | The job crashed 
    MsgJobCrashed JobId
  | -- | The job crashed
    MsgJobTimeout JobId
  | -- | Immediately reply the status of the job.
    MsgJobStatus JobId (ReplyChan JobStatus)
  | -- | Reply when the job is done.
    MsgJobWait JobId (ReplyChan JobDoneReason)
  | -- | Some time has passed.
    MsgTick
  | -- | Add a new worker.
    MsgWorkerAdd WorkerName (IO (Server WorkerMsg)) (ReplyChan (Either String Worker))
  | -- | Worker reports that it has completed a job.
    MsgWorkerDone JobId
  | -- | Worker reports that it is idle.
    MsgWorkerIdle WorkerName
  | -- | Worker reports that it is stopping.
    MsgWorkerGone WorkerName

-- | A handle to the SPC instance.
data SPC = SPC (Server SPCMsg)

-- | A handle to a worker.
data Worker = Worker (Server WorkerMsg)

-- | The central state. Must be protected from the bourgeoisie.
data SPCState = SPCState
  { spcJobsPending :: [(JobId, Job)],
    spcJobsRunning :: [(JobId, Job)],
    spcJobsDone :: [(JobId, JobDoneReason)],
    spcJobCounter :: JobId,
    spcIdleWorkers :: [(WorkerName, Worker)],
    spcBusyWorkers :: [(WorkerName, (JobId, Seconds, Worker))]
  }

-- | The monad in which the main SPC thread runs. This is a state
-- monad with support for IO.
newtype SPCM a = SPCM (SPCState -> IO (a, SPCState))

instance Functor SPCM where
  fmap = liftM

instance Applicative SPCM where
  pure x = SPCM $ \state -> pure (x, state)
  (<*>) = ap

instance Monad SPCM where
  SPCM m >>= f = SPCM $ \state -> do
    (x, state') <- m state
    let SPCM f' = f x
    f' state'

-- | Retrieve the state.
get :: SPCM SPCState
get = SPCM $ \state -> pure (state, state)

-- | Overwrite the state.
put :: SPCState -> SPCM ()
put state = SPCM $ \_ -> pure ((), state)

-- | Modify the state.
modify :: (SPCState -> SPCState) -> SPCM ()
modify f = do
  state <- get
  put $ f state

-- | Lift an 'IO' action into 'SPCM'.
io :: IO a -> SPCM a
io m = SPCM $ \state -> do
  x <- m
  pure (x, state)

-- | Run the SPCM monad.
runSPCM :: SPCState -> SPCM a -> IO a
runSPCM state (SPCM f) = fst <$> f state

sendToWorker :: Worker -> WorkerMsg -> IO ()
sendToWorker worker msg = sendTo (getWorkerServer worker) msg

schedule :: SPCM ()
schedule = do 
  state <- get
  case (spcJobsPending state, spcIdleWorkers state) of
    ((jobId, job) : pendingJobs, (workerName, worker) : idleWorkers) -> do
        io $ sendToWorker worker $ MsgDoJob jobId job
        now <- io $ getSeconds
        let deadline = now + fromIntegral (jobMaxSeconds job)
        modify $ \st ->
          st
            { spcJobsPending = pendingJobs,
              spcJobsRunning = (jobId, job) : spcJobsRunning st,
              spcIdleWorkers = idleWorkers,
              spcBusyWorkers = (workerName, (jobId, deadline, worker)) : spcBusyWorkers st
            }
        schedule -- Try to schedule more jobs
    _ -> return ()

jobDone :: JobId -> JobDoneReason -> SPCM ()
jobDone jobId reason = do
  modify $ \state ->
    state
      { spcJobsRunning = removeAssoc jobId (spcJobsRunning state),
        spcJobsDone = (jobId, reason) : spcJobsDone state
      }

workerIsIdle :: WorkerName -> Worker -> SPCM ()
workerIsIdle workerName worker = do
  modify $ \state ->
        state
          { spcBusyWorkers = removeAssoc workerName (spcBusyWorkers state),
            spcIdleWorkers = (workerName, worker) : spcIdleWorkers state
          }

workerIsGone :: WorkerName -> SPCM ()
workerIsGone workerName2 = do
  modify $ \state ->
    state
      { spcIdleWorkers = removeAssoc workerName2 (spcIdleWorkers state),
        spcBusyWorkers = removeAssoc workerName2 (spcBusyWorkers state)
      } 

getWorkerServer :: Worker -> Server WorkerMsg
getWorkerServer (Worker server) = server  

findWorkerFromJobid :: JobId -> SPCState -> Maybe (WorkerName, (JobId, Seconds, Worker))
findWorkerFromJobid jobid state = do
  find (\(_, (workerJobId, _, _)) -> workerJobId == jobid) (spcBusyWorkers state)

checkTimeouts :: SPCM ()
checkTimeouts = do
  state <- get
  now <- io getSeconds
  case spcJobsRunning state of
    [] -> pure ()
    jobs -> forM_ jobs $ \(jobid, _job) -> do
      let busyWorker = findWorkerFromJobid jobid state
      case busyWorker of
        Just (workerName, (_, deadline, worker)) -> do
          when (now >= deadline) $ do
            io $ sendToWorker worker $ MsgTimeout workerName jobid
        Nothing -> pure ()
  return ()

workerExists :: WorkerName -> SPCM Bool
workerExists workerName2 = do
  state <- get
  let idleNames = map fst (spcIdleWorkers state)
      busyNames = map fst (spcBusyWorkers state)
  pure $ workerName2 `elem` (idleNames ++ busyNames)

handleMsg :: Chan SPCMsg -> SPCM ()
handleMsg c = do
  checkTimeouts
  schedule
  msg <- io $ receive c
  case msg of
    MsgJobAdd job rsvp -> do
      state <- get
      let JobId jobid = spcJobCounter state
      put $
        state
          { spcJobsPending =
              (JobId jobid, job) : spcJobsPending state,
            spcJobCounter = JobId $ succ jobid
          }
      io $ reply rsvp $ JobId jobid
    MsgJobStatus jobid rsvp -> do
      state <- get
      io $ reply rsvp $ case ( lookup jobid $ spcJobsPending state,
                               lookup jobid $ spcJobsRunning state,
                               lookup jobid $ spcJobsDone state
                             ) of
        (Just _, _, _) -> JobPending
        (_, Just _, _) -> JobRunning
        (_, _, Just r) -> JobDone r
        _ -> JobUnknown
        
    MsgTick ->
      pure ()

    MsgWorkerAdd workerName2 worker2 rsvp -> do
      exists <- workerExists workerName2
      if exists
        then io $ reply rsvp $ Left "Worker name already exists"
        else do
          worker <- io $ worker2
          modify $ \state ->
            state { spcIdleWorkers = (workerName2, Worker worker) : spcIdleWorkers state }
          io $ reply rsvp $ Right $ Worker worker
    MsgWorkerDone jobId -> do
      jobDone jobId Done
    MsgWorkerIdle workerName -> do
      state <- get
      case lookup workerName $ spcBusyWorkers state of
        Just (_, _, worker) -> 
          workerIsIdle workerName worker
        Nothing -> pure ()
    MsgWorkerGone workerName -> do
      state <- get
      case lookup workerName $ (spcBusyWorkers state) of
        Just (jobid, _, _) -> do
          modify $ \st ->
                      st
                        { spcJobsRunning = removeAssoc jobid $ spcJobsRunning st,
                          spcJobsDone = (jobid, DoneCancelled) : spcJobsDone st
                        }
        Nothing -> pure ()
      workerIsGone workerName
    MsgJobCancel jobid -> do
      state <- get
      case lookup jobid $ spcJobsPending state of
        Nothing -> do-- Find worker who has the job
          let worker = findWorkerFromJobid jobid state in
            case worker of
              Just (_, (_, _, worker2)) -> do
                io $ sendToWorker worker2 $ MsgCancel jobid
                jobDone jobid DoneCancelled
                modify $ \st ->
                  st
                    { spcJobsRunning = removeAssoc jobid $ spcJobsRunning st,
                      spcJobsDone = (jobid, DoneCancelled) : spcJobsDone st
                    }
              Nothing -> pure ()
        Just _ -> 
          modify $ \st ->
            st
              { spcJobsPending = removeAssoc jobid $ spcJobsPending st,
                spcJobsDone = (jobid, DoneCancelled) : spcJobsDone st
              }  
    MsgJobCrashed jobid -> do
      state <- get
      case lookup jobid $ spcJobsRunning state of
        Just _ -> do
            jobDone jobid DoneCrashed
        Nothing -> pure ()
    MsgJobTimeout jobid -> do
      state <- get
      case lookup jobid $ spcJobsRunning state of
        Just _ -> do
            jobDone jobid DoneTimeout
        Nothing -> pure ()
    _ -> return ()

startSPC :: IO SPC
startSPC = do
  let initial_state =
        SPCState
          { spcJobCounter = JobId 0,
            spcJobsPending = [],
            spcJobsRunning = [],
            spcJobsDone = [],
            spcIdleWorkers = [],
            spcBusyWorkers = []
          }
  spcServer <- spawn $ \c -> do
    void $ forkIO $ timer c
    runSPCM initial_state $ forever $ handleMsg c
  pure $ SPC spcServer
  where
    timer c = forever $ do
      threadDelay 1000000
      send c MsgTick

-- | Add a job for scheduling.
jobAdd :: SPC -> Job -> IO JobId
jobAdd (SPC c) job =
  requestReply c $ MsgJobAdd job

-- | Asynchronously query the job status.
jobStatus :: SPC -> JobId -> IO JobStatus
jobStatus (SPC c) jobid =
  requestReply c $ MsgJobStatus jobid

-- | Synchronously block until job is done and return the reason.
jobWait :: SPC -> JobId -> IO JobDoneReason
jobWait (SPC c) jobid =
  requestReply c $ MsgJobWait jobid

-- | Asynchronously cancel a job.
jobCancel :: SPC -> JobId -> IO ()
jobCancel (SPC c) jobid =
  sendTo c $ MsgJobCancel jobid

-- | Shut down a running worker. No effect if the worker is already
-- terminated.
workerStop :: Worker -> IO ()
workerStop worker = sendToWorker worker MsgStop

-- | Add a new worker with this name. Fails with 'Left' if a worker
-- with that name already exists.
workerAdd :: SPC -> WorkerName -> IO (Either String Worker)
workerAdd (SPC c) workerName = do
  request <- requestReply c $ MsgWorkerAdd workerName $ spawn idle
  case request of
    Left e -> pure $ Left e
    Right worker -> pure $ Right worker
  where
    idle worker_chan = do
      msg <- receive worker_chan
      case msg of
        MsgStop -> do
          sendTo c $ MsgWorkerGone workerName
          return ()
        MsgCancel _jobId -> do
          idle worker_chan
        MsgDoJob jobId job -> do
          tid <- forkIO $ do
            let doJob = do
                  jobAction job
                  send worker_chan $ MsgJobDone workerName jobId
                onException :: SomeException -> IO ()
                onException _ = do
                  send worker_chan $ MsgCrashed workerName jobId
            doJob `catch` onException
          working worker_chan tid
        MsgCrashed _workerName _jobId -> do
          idle worker_chan
        MsgJobDone _workerName _jobId -> 
          idle worker_chan
        MsgTimeout workerName jobId -> do
          idle worker_chan
    working worker_chan tid = do
      msg <- receive worker_chan
      case msg of
        MsgStop -> do
          sendTo c $ MsgWorkerGone workerName
          killThread tid
          return ()
        MsgCancel _jobId -> do
          killThread tid
          sendTo c $ MsgWorkerIdle workerName
          idle worker_chan
        MsgDoJob _jobId _job -> do
          working worker_chan tid
        MsgCrashed workerName jobId -> do
          sendTo c $ MsgWorkerIdle workerName
          sendTo c $ MsgJobCrashed jobId
          idle worker_chan
        MsgJobDone workerName jobId -> do
          sendTo c $ MsgWorkerDone jobId
          sendTo c $ MsgWorkerIdle workerName
          idle worker_chan
        MsgTimeout workerName jobId -> do
          sendTo c $ MsgJobTimeout jobId
          sendTo c $ MsgWorkerIdle workerName
          idle worker_chan