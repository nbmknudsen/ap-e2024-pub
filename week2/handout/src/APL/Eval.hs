module APL.Eval
  ( Val (..),
    eval,
    runEval,
    Error,
    Env,
  )
where

import APL.AST (Exp (..), VName)
import Control.Monad (ap, liftM)

data Val
  = ValInt Integer
  | ValBool Bool
  | ValFun Env VName Exp
  deriving (Eq, Show)

type Env = [(VName, Val)]

envEmpty :: Env
envEmpty = []

envExtend :: VName -> Val -> Env -> Env
envExtend v val env = (v, val) : env

envLookup :: VName -> Env -> Maybe Val
envLookup v env = lookup v env

type Error = String

newtype EvalM a = EvalM (Env -> Either Error a)

-- Functor f
-- fmap :: (a -> b) -> f a -> f b
instance Functor EvalM where
  fmap f (EvalM x) =
    EvalM $ \env -> case x env of
      Right v -> Right $ f v
      Left err -> Left err

-- Functor f
-- pure :: a -> f a
-- (<*>) :: f (a -> b) -> f a -> f b
instance Applicative EvalM where
  pure x = EvalM $ \_env -> Right x
  EvalM x <*> EvalM y =
    EvalM $ \env -> case (x env, y env) of
      (Left err, _)      -> Left err
      (_, Left err)      -> Left err
      (Right f, Right a) -> Right (f a)


-- Monad m
-- (>>=) :: m a -> (a -> m b) -> m b
instance Monad EvalM where
  EvalM x >>= f = 
    EvalM $ \env -> case x env of
      Left err -> Left err
      Right a -> 
        let EvalM y = f a 
          in y env

askEnv :: EvalM Env
askEnv = EvalM $ \env -> Right env

localEnv :: (Env -> Env) -> EvalM a -> EvalM a
localEnv f (EvalM m) = EvalM $ \env -> m (f env)

failure :: String -> EvalM a
failure err = EvalM $ \_env -> Left err

runEval :: EvalM a -> Either Error a
runEval (EvalM x) = x envEmpty

catch :: EvalM a -> EvalM a -> EvalM a
catch (EvalM m1) (EvalM m2) = EvalM $ \env -> 
  case m1 env of
    Left _  -> m2 env
    Right a -> Right a


bin_op :: (Integer -> Integer -> Integer) -> Exp -> Exp -> EvalM Val
bin_op operator e1 e2 = do
  x <- eval e1
  y <- eval e2
  case (x, y) of
    (ValInt x', ValInt y') -> pure $ ValInt $ x' `operator` y'
    _ -> failure "Non-integer operand"

eval :: Exp -> EvalM Val
eval (CstInt x) = pure $ ValInt x
eval (CstBool x) = pure $ ValBool x

eval (Add e1 e2) = bin_op (+) e1 e2
eval (Sub e1 e2) = bin_op (-) e1 e2
eval (Mul e1 e2) = bin_op (*) e1 e2

eval (Div e1 e2) = do
  x <- eval e1
  y <- eval e2
  case (x, y) of
    (_, ValInt 0) -> failure "Division by zero"
    (ValInt x', ValInt y') -> pure $ ValInt $ x' `div` y'
    _ -> failure "Non-integer operand"

eval (Pow e1 e2) = do
  x <- eval e1
  y <- eval e2
  case (x, y) of
    (ValInt x', ValInt y') 
        | y' < 0      -> failure "Negative exponent"
        | otherwise   -> pure $ ValInt $ x' ^ y'
    _ -> failure "Non-integer operand"

eval (Eql e1 e2) = do
  x <- eval e1
  y <- eval e2
  case (x, y) of
    (ValInt x', ValInt y') -> pure $ ValBool $ x' == y'
    (ValBool x', ValBool y') -> pure $ ValBool $ x' == y'
    (_, _) -> failure "Invalid operands to equality"

eval (If cond e2 e3) = do
  cond' <- eval cond
  case (cond') of
    ValBool True  -> eval e2
    ValBool False -> eval e3
    _ -> failure "Non-boolean condition"

eval (Var n) = do
  env <- askEnv
  case (envLookup n env) of
    Just val -> pure val
    Nothing -> failure $ "Unknown variable: " ++ n

eval (Let var e1 e2) = do
  val <- eval e1
  localEnv (envExtend var val) (eval e2)

eval (Lambda var e1) = do
  env <- askEnv
  pure $ ValFun env var e1

eval (Apply e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (ValFun env2 var e3, val) -> localEnv (const $ envExtend var val env2) (eval e3)
    (_, _) -> failure "Cannot apply non-function"

eval (TryCatch e1 e2) = do
  eval e1 `catch` eval e2
