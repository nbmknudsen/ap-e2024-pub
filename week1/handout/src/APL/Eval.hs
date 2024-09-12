module APL.Eval
  (
  Val(..), eval, envEmpty
  )
where

import APL.AST (Exp(..), VName)

data Val
  = ValInt Integer
  | ValBool Bool
  deriving (Eq, Show)

type Error = String

type Env = [(VName, Val)]

-- | Empty environment, which contains no variable bindings.
envEmpty :: Env
envEmpty = []

-- | Extend an environment with a new variable binding,
-- producing a new environment.
envExtend :: VName -> Val -> Env -> Env
envExtend name value env = (name, value) : env

-- | Look up a variable name in the provided environment.
-- Returns Nothing if the variable is not in the environment.
envLookup :: VName -> Env -> Maybe Val
envLookup name env = lookup name env

eval_helper :: (Integer -> Integer -> Integer) -> Env -> Exp -> Exp -> Either Error Val
eval_helper operator env e1 e2 =
  case (eval env e1, eval env e2) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right (ValInt x), Right (ValInt y)) -> Right $ ValInt $ x `operator` y
    (Right _, Right _) -> Left "Wrong type"


eval :: Env -> Exp -> Either Error Val
eval _env (CstInt x) = Right $ (ValInt x)
eval env (Add e1 e2) = eval_helper (+) env e1 e2
eval env (Sub e1 e2) = eval_helper (-) env e1 e2
eval env (Mul e1 e2) = eval_helper (*) env e1 e2

eval env (Div e1 e2) =
  case (eval env e1, eval env e2) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (_, Right (ValInt 0)) -> Left "Division by zero"
    (Right (ValInt x), Right (ValInt y)) -> Right $ ValInt $ x `div` y
    (Right _, Right _) -> Left "Wrong type"

eval env (Pow e1 e2) =
  case (eval env e1, eval env e2) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right (ValInt x), Right (ValInt y)) 
        | y < 0     -> Left "Negative exponent"
        | otherwise -> Right $ ValInt $ x ^ y
    (Right _, Right _) -> Left "Wrong type"

eval _env (CstBool x) = Right $ (ValBool x)

eval env (Eql e1 e2) =
  case (eval env e1, eval env e2) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right (ValInt x), Right (ValInt y)) -> Right $ ValBool $ x == y
    (Right (ValBool x), Right (ValBool y)) -> Right $ ValBool $ x == y
    (Right _, Right _) -> Left "Different types"

eval env (If e1 e2 e3) =
  case (eval env e1) of
    Left err -> Left err
    Right (ValBool True) -> eval env e2
    Right (ValBool False) -> eval env e3
    Right _ -> Left "Condition is non-boolean"

eval env (Var n) =
  case (envLookup n env) of
    Just x -> Right x
    Nothing -> Left $ "Unknown variable: " ++ n

eval env (Let n e1 e2) =
  case (eval env e1) of
    Left err -> Left err
    Right x -> eval (envExtend n x env) e2

