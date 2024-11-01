module SPC
  ( -- * SPC startup
    SPC,
    startSPC,
    pingSPC,
    Job(..),
    JobId,
    jobAdd,
    JobDoneReason(..),
    JobStatus(..),
    jobStatus,
    jobCancel,
    jobWait,
  )
where

import Control.Concurrent
  ( ThreadId,
    forkIO,
    killThread,
    threadDelay,
  )
import Control.Exception (SomeException, catch)
import Control.Monad (ap, forM_, forever, liftM, void)
import Data.List (partition)
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
-- | A Handle to the SPC instance.
data SPC = SPC (Server SPCMsg)

newtype SPCM a = SPCM (SPCState -> IO (a, SPCState))

-- Functor f
-- fmap :: (a -> b) -> f a -> f b
instance Functor SPCM where
  fmap = liftM

-- Functor f
-- pure :: a -> f a
-- (<*>) :: f (a -> b) -> f a -> f b
instance Applicative SPCM where
  pure x = SPCM $ \state -> pure (x, state)
  (<*>) = ap

  -- Monad m
-- (>>=) :: m a -> (a -> m b) -> m b
instance Monad SPCM where
  SPCM m >>= f = SPCM $ \state -> do
    (x, state') <- m state
    let SPCM f' = f x
    f' state'

get :: SPCM SPCState
get = SPCM $ \state -> pure (state, state)

put :: SPCState -> SPCM ()
put state = SPCM $ \_ -> pure ((), state)

io :: IO a -> SPCM a
io m = SPCM $ \state -> do
  x <- m
  pure (x, state)

runSPCM :: SPCState -> SPCM a -> IO a
runSPCM state (SPCM f) = fst <$> f state


-- Messages sent to SPC.
data SPCMsg = MsgPing (ReplyChan Int)
              | -- | Add the job, and reply with the job ID.
                MsgJobAdd Job (ReplyChan JobId)
              | -- | Immediately reply the status of the job.
                MsgJobStatus JobId (ReplyChan (Maybe JobStatus))
              | -- | Cancel the job with the given job ID
                MsgJobCancel JobId
              |  -- | Jobs waiting to be finished, and reply with the JobDoneReason. 
                MsgJobWait JobId (ReplyChan (Maybe JobDoneReason))
              | -- | Job with job ID is done
                MsgJobDone JobId
              | -- | Job crashed
                MsgJobCrashed JobId
              | -- | Trivial message with no payload
                MsgTick


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
  | -- | The job was explicitly cancelled.
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
  deriving (Eq, Ord, Show)

-- | The central state. Must be protected from the bourgeoisie.
data SPCState = SPCState
  { spcJobCounter :: JobId,
    spcJobsPending :: [(JobId, Job)],
    spcJobsDone :: [(JobId, JobDoneReason)],
    spcWaiting :: [(JobId, ReplyChan (Maybe JobDoneReason))],
    spcChan :: Chan SPCMsg,
    spcJobRunning :: Maybe (JobId, Seconds, ThreadId)
  }

-- | Add a job for scheduling.
jobAdd :: SPC -> Job -> IO JobId
jobAdd (SPC c) job =
  requestReply c $ MsgJobAdd job

-- | Query the job status. Returns 'Nothing' if job is not known to
-- this SPC instance.
jobStatus :: SPC -> JobId -> IO (Maybe JobStatus)
jobStatus (SPC c) jobid = do
  requestReply c $ MsgJobStatus jobid

-- | Asynchronously cancel a job.
jobCancel :: SPC -> JobId -> IO ()
jobCancel (SPC c) jobid =
  sendTo c $ MsgJobCancel jobid

-- | Synchronously block until job is done and return the reason.
-- Returns 'Nothing' if job is not known to this SPC instance.
jobWait :: SPC -> JobId -> IO (Maybe JobDoneReason)
jobWait (SPC c) jobid =
  requestReply c $ MsgJobWait jobid

jobDone :: JobId -> JobDoneReason -> SPCM ()
jobDone jobid reason = do
  state <- get
  case lookup jobid $ spcJobsDone state of
    Just _ ->
      -- We already know this job is done.
      pure ()
    Nothing -> do
      let (waiting_for_job, not_waiting_for_job) =
            partition ((== jobid) . fst) (spcWaiting state)
      forM_ waiting_for_job $ \(_, rsvp) ->
        io $ reply rsvp $ Just reason
      put $
        state
          { spcWaiting = not_waiting_for_job,
            spcJobsDone = (jobid, reason) : spcJobsDone state,
            spcJobRunning = Nothing,
            spcJobsPending = removeAssoc jobid $ spcJobsPending state
          }


schedule :: SPCM ()
schedule = do
  state <- get
  case (spcJobRunning state, spcJobsPending state) of
    (Nothing, (jobid, job) : jobs) -> do
      t <- io $ forkIO $ do
        let doJob = do
              jobAction job
              send (spcChan state) $ MsgJobDone jobid
            onException :: SomeException -> IO ()
            onException _ =
              send (spcChan state) $ MsgJobCrashed jobid
        doJob `catch` onException
      now <- io $ getSeconds
      let deadline = now + fromIntegral (jobMaxSeconds job)
      put $
        state
          { spcJobRunning = Just (jobid, deadline, t),
            spcJobsPending = jobs
          }
    _ -> pure ()

checkTimeouts :: SPCM ()
checkTimeouts = do
  state <- get
  now <- io getSeconds
  case spcJobRunning state of
    Just (jobid, deadline, tid)
      | now >= deadline -> do
          io $ killThread tid
          jobDone jobid DoneTimeout
    _ -> pure ()

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
              (spcJobCounter state, job) : spcJobsPending state,
            spcJobCounter = JobId $ succ jobid
          }
      io $ reply rsvp $ JobId jobid
    MsgJobStatus jobid rsvp -> do
      state <- get
      io $ reply rsvp $ case ( lookup jobid $ spcJobsPending state,
                               spcJobRunning state,
                               lookup jobid $ spcJobsDone state
                             ) of
        (Just _, _, _) -> Just JobPending
        (_, Just (running_job, _, _), _)
          | running_job == jobid ->
              Just $ JobRunning
        (_, _, Just r) -> Just $ JobDone r
        _ -> Nothing
    MsgJobCancel cancel_jobid -> do
      state <- get
      case spcJobRunning state of
        Just (jobid, _, tid) | jobid == cancel_jobid -> do
          io $ killThread tid
          jobDone jobid DoneCancelled
        _ -> pure ()
    MsgJobWait jobid rsvp -> do
      state <- get
      case lookup jobid $ spcJobsDone state of
        Just reason -> do
          io $ reply rsvp $ Just reason
        Nothing ->
          put $ state {spcWaiting = (jobid, rsvp) : spcWaiting state}
    MsgJobDone done_jobid -> do
      state <- get
      case spcJobRunning state of
        Just (jobid, _, _)
          | jobid == done_jobid ->
              jobDone jobid Done
        _ -> pure ()
    MsgJobCrashed crashed_jobid -> do
      state <- get
      case spcJobRunning state of
        Just (jobid, _, tid) | jobid == crashed_jobid ->
          jobDone jobid DoneCrashed
        _ -> pure ()
    MsgTick ->
      pure ()
   

startSPC :: IO SPC
startSPC = do
  let initial_state c =
        SPCState
          { spcJobCounter = JobId 0
          , spcJobsPending = []
          , spcJobsDone = []
          , spcWaiting = []
          , spcChan = c
          , spcJobRunning = Nothing
          }
  server <- spawn $ \c -> runSPCM (initial_state c) $ forever $ handleMsg c
  void $ spawn $ timer server
  pure $ SPC server
  where
    timer server _ = forever $ do
      threadDelay 1000000 -- 1 second
      sendTo server MsgTick

-- Not used anymore  
pingSPC :: SPC -> IO Int
pingSPC (SPC c) = requestReply c MsgPing


