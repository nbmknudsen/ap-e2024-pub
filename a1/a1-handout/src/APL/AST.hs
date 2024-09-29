module APL.AST
  ( VName,
    Exp (..),
    printExp,
  )
where

type VName = String

data Exp
  = CstInt Integer
  | CstBool Bool
  | Add Exp Exp
  | Sub Exp Exp
  | Mul Exp Exp
  | Div Exp Exp
  | Pow Exp Exp
  | Eql Exp Exp
  | If Exp Exp Exp
  | Var VName
  | Let VName Exp Exp
  | Lambda VName Exp
  | Apply Exp Exp
  | TryCatch Exp Exp
  -- TODO: add cases
  deriving (Eq, Show)

-- "Unknown variable: " ++ e
printExp :: Exp -> String
printExp e = 
  case e of
    (Var n) -> n
    (CstInt n) -> show n
    (CstBool n) -> show n
    (Add x y) -> (printExp x ++ " + " ++ printExp y)
    (Sub x y) -> (printExp x ++ " - " ++ printExp y)
    (Mul x y) -> (printExp x ++ " * " ++ printExp y)
    (Div x y) -> (printExp x ++ " / " ++ printExp y)
    (Pow x y) -> (printExp x ++ " ** " ++ printExp y)
    (Eql x y) -> (printExp x ++ " == " ++ printExp y)
    (If x y z) -> ("if " ++ printExp x ++ " then " ++ printExp y ++ " else " ++ printExp z)
    (Let x y z) -> ("let " ++ printExp (Var x) ++ " = " ++ printExp y ++ " in " ++ printExp z)
    (Lambda var y) ->  "\\" ++ printExp (Var var) ++ " -> " ++ printExp y
    (Apply x y) -> (printExp x ++ " " ++ printExp y)
    (TryCatch x y) ->  ("try " ++ printExp x ++ " catch " ++ printExp y)
    