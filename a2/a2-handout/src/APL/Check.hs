module APL.Check (checkExp, Error) where

import APL.AST (Exp (..), VName)

import Control.Monad (ap, liftM)

type Error = String

type Env = [(VName, Exp)]

newtype CheckM a = CheckM (Env -> Either Error a)

askEnv :: CheckM Env
askEnv = CheckM $ \env -> Right env

localEnv :: (Env -> Env) -> CheckM a -> CheckM a
localEnv f (CheckM m) = CheckM $ \env -> m (f env)

envEmpty :: Env
envEmpty = []

envExtend :: VName -> Exp -> Env -> Env
envExtend v val env = (v, val) : env

envLookup :: VName -> Env -> Maybe Exp
envLookup v env = lookup v env

instance Functor CheckM where
    fmap = liftM

instance Applicative CheckM where
    pure x = CheckM $ \_env -> Right x
    (<*>) = ap

instance Monad CheckM where
    CheckM x >>= f = CheckM $ \env ->
        case x env of
           Left err -> Left err
           Right x' -> 
            let CheckM y = f x'
             in y env

runCheck :: CheckM a -> Either Error a
runCheck (CheckM m) = m envEmpty


checkTwoExp :: Exp -> Exp -> CheckM (Exp -> Maybe Error)
checkTwoExp e1 e2 = do
    f1 <- check e1
    f2 <- check e2
    case (f1 e1, f2 e2) of
        (Just err1, Just err2) -> pure $ \_ -> Just (err1 ++ " and " ++ err2)
        (Just err, _) -> pure $ \_ -> Just err
        (_, Just err) -> pure $ \_ -> Just err
        (_, _)        -> pure $ \_ -> Nothing

checkSingleExp :: Exp -> CheckM (Exp -> Maybe Error)
checkSingleExp e = do
    f <- check e
    case f e of
        Just err -> pure $ \_ -> Just err
        Nothing  -> pure $ \_ -> Nothing

checkExp :: Exp -> Maybe Error
checkExp e1 =
  case runCheck (check e1) of
    Left err -> Just err
    Right f -> f e1

check :: Exp -> CheckM (Exp -> Maybe Error)
check (CstInt _) =  pure $ \_ ->  Nothing

check (CstBool _) = pure $ \_ ->  Nothing

check (Add e1 e2) = checkTwoExp e1 e2

check (Sub e1 e2) = checkTwoExp e1 e2

check (Mul e1 e2) = checkTwoExp e1 e2

check (Div e1 e2) = checkTwoExp e1 e2

check (Pow e1 e2) = checkTwoExp e1 e2

check (Eql e1 e2) = checkTwoExp e1 e2

check (If cond e1 e2) = do
    cond' <- check cond
    f1 <- check e1
    f2 <- check e2
    pure $ \_ ->
        case cond' cond of
            Just err -> Just err
            Nothing ->
                case f1 e1 of
                    Just err -> Just err
                    Nothing -> f2 e2

check (Var v) = do
    env <- askEnv 
    case envLookup v env of
        Nothing  -> pure $ \_ -> Just ("Unknown variable: " ++ v)
        Just _   -> pure $ \_ -> Nothing

check (Let var e1 e2) = do
    f1 <- check e1
    f2 <- localEnv (envExtend var e1) (check e2)
    pure $ \_ ->
        case f1 e1 of
            Just err -> Just err
            Nothing -> 
                case f2 e2 of
                    Just err -> Just err
                    Nothing -> Nothing

check (Lambda var body) = do
    f1 <- localEnv (envExtend var (CstInt 0)) (check body)
    pure $ \_ ->
        case f1 body of
            Just err -> Just err
            Nothing -> Nothing

check (Apply e1 e2) = checkTwoExp e1 e2

check (TryCatch e1 e2) = checkTwoExp e1 e2

check (Print _ e) = checkSingleExp e

check (KvPut e1 e2) = checkTwoExp e1 e2

check (KvGet e) = checkSingleExp e
