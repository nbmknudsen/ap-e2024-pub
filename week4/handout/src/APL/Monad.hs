module APL.Monad
  ( envEmpty,
    envExtend,
    envLookup,
    stateInitial,
    askEnv,
    modifyEffects,
    localEnv,
    getState,
    putState,
    modifyState,
    evalKvGet,
    evalKvPut,
    evalPrint,
    failure,
    catch,
    EvalM,
    Val (..),
    EvalOp (..),
    Free (..),
    Error,
    Env,
    State,
  )
where

import APL.AST (Exp (..), VName)
import Control.Monad (ap)

data Val
  = ValInt Integer
  | ValBool Bool
  | ValFun Env VName Exp
  deriving (Eq, Show)

type Error = String

type Env = [(VName, Val)]

envEmpty :: Env
envEmpty = []

envExtend :: VName -> Val -> Env -> Env
envExtend v val env = (v, val) : env

envLookup :: VName -> Env -> Maybe Val
envLookup v env = lookup v env

type State = [(Val, Val)]

stateInitial :: State
stateInitial = []

data Free e a
  = Pure a
  | Free (e (Free e a))

-- fmap :: (a -> b) -> Free e a -> Free e b
instance (Functor e) => Functor (Free e) where
  fmap f (Pure x) = Pure (f x)
  -- g :: e (Free e a)
  -- f :: a -> b
  fmap f (Free g) = Free $ fmap (fmap f) g
  -- (fmap f) = f0 a -> f0 b
  -- fmap (fmap f) g = e (Free e b)
  -- Free $ fmap (fmap f) g = Free e (Free e b), where b = (Free e b)

-- pure :: a -> f a
-- (<*>) :: f (a -> b) -> f a -> f b
instance (Functor e) => Applicative (Free e) where
  pure = Pure
  (<*>) = ap

-- (>>=) :: Free e a -> (a -> Free e b) -> Free e b
instance (Functor e) => Monad (Free e) where
  Pure x >>= f = f x
  -- f :: a -> Free e b
  -- g :: e (Free e a)
  -- Free g :: e (Free e a)
  -- h :: Free e a -> Free e b
  Free g >>= f = Free $ h <$> g
    where
      h x = x >>= f
  -- x = Free e a
  -- h <$> g = e (Free e a)
  -- Free $ h <$> g Free e (Free e a), where b = (Free e a)

data EvalOp a
  = ReadOp (Env -> a)
  | StateGetOp (State -> a)
  | StatePutOp State a

-- fmap :: (a -> b) -> EvalOp a -> EvalOp b
instance Functor EvalOp where
  -- f :: a -> b
  -- k :: Env -> a
  fmap f (ReadOp k) = ReadOp $ fmap f k
  -- fmap f k = f . k
  fmap f (StateGetOp k) = StateGetOp $ fmap f k
  fmap f (StatePutOp s a) = StatePutOp s $ f a

type EvalM a = Free EvalOp a

askEnv :: EvalM Env
askEnv = Free $ ReadOp $ \env -> pure env

modifyEffects :: (Functor e, Functor h) => (e (Free e a) -> h (Free e a)) -> Free e a -> Free h a
modifyEffects _ (Pure x) = Pure x
modifyEffects g (Free e) = error "TODO"

localEnv :: (Env -> Env) -> EvalM a -> EvalM a
localEnv f m = error "TODO"

getState :: EvalM State
getState = Free $ StateGetOp $ \state -> pure state

putState :: State -> EvalM ()
putState s = Free $ StatePutOp s $ pure ()

modifyState :: (State -> State) -> EvalM ()
modifyState f = do
  s <- getState
  putState $ f s

evalPrint :: String -> EvalM ()
evalPrint = error "TODO"

failure :: String -> EvalM a
failure = error "TODO"

catch :: EvalM a -> EvalM a -> EvalM a
catch = error "To be completed in assignment 4."

evalKvGet :: Val -> EvalM Val
evalKvGet = error "To be completed in assignment 4."

evalKvPut :: Val -> Val -> EvalM ()
evalKvPut = error "To be completed in assignment 4."
