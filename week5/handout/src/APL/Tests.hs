module APL.Tests where

import APL.AST (Exp (..), VName)
import APL.Eval (eval, runEval)
import Test.QuickCheck (Gen, oneof, elements, listOf, Arbitrary (arbitrary, shrink), sized, sample, quickCheck)

-- to test: sample $ genVar
genVar :: Gen VName
genVar = do
    alpha <- elements ['a' .. 'z']
    alphaNums <- listOf $ elements $ ['a' .. 'z'] ++ ['0' .. '9']
    pure (alpha : alphaNums)

instance Arbitrary Exp where
  arbitrary = sized genExp

  shrink (Add e1 e2) =
    e1 : e2 : [Add e1' e2 | e1' <- shrink e1] ++ [Add e1 e2' | e2' <- shrink e2]
  shrink (Sub e1 e2) =
    e1 : e2 : [Sub e1' e2 | e1' <- shrink e1] ++ [Sub e1 e2' | e2' <- shrink e2]
  shrink (Mul e1 e2) =
    e1 : e2 : [Mul e1' e2 | e1' <- shrink e1] ++ [Mul e1 e2' | e2' <- shrink e2]
  shrink (Div e1 e2) =
    e1 : e2 : [Div e1' e2 | e1' <- shrink e1] ++ [Div e1 e2' | e2' <- shrink e2]
  shrink (Pow e1 e2) =
    e1 : e2 : [Pow e1' e2 | e1' <- shrink e1] ++ [Pow e1 e2' | e2' <- shrink e2]
  shrink (Eql e1 e2) =
    e1 : e2 : [Eql e1' e2 | e1' <- shrink e1] ++ [Eql e1 e2' | e2' <- shrink e2]
  shrink (If cond e1 e2) =
    e1 : e2 : [If cond' e1 e2 | cond' <- shrink cond] ++ [If cond e1' e2 | e1' <- shrink e1] ++ [If cond e1 e2' | e2' <- shrink e2]
  shrink (Var x) =
    [Var x' | x' <- shrink x, not (null x')]
  shrink (Let x e1 e2) =
    e1 : [Let x e1' e2 | e1' <- shrink e1] ++ [Let x e1 e2' | e2' <- shrink e2]
  shrink (Lambda x e) =
    e : [Lambda x e' | e' <- shrink e]
  shrink (TryCatch e1 e2) =
    e1 : e2 : [TryCatch e1' e2 | e1' <- shrink e1] ++ [TryCatch e1 e2' | e2' <- shrink e2]
  shrink _ = []
    
-- to test: sample $ genExp
-- sample (sized genExp)
genExp :: Int -> Gen Exp
genExp size = 
    if size <= 1
    then Var <$> genVar
    else
        oneof 
            [CstInt <$> arbitrary
            , CstBool <$> arbitrary
            , Var <$> genVar
            , Add <$> genExp half <*> genExp half
            , Sub <$> genExp half <*> genExp half
            , Mul <$> genExp half <*> genExp half
            , Div <$> genExp half <*> genExp half
            , Pow <$> genExp half <*> genExp half
            , Eql <$> genExp half <*> genExp half
            , If <$> genExp half <*> genExp half <*> genExp half
            , Let <$> genVar <*> genExp half <*> genExp half
            , Lambda <$> genVar <*> genExp (size - 1)
            , Apply <$> genExp half <*> genExp half
            , TryCatch <$> genExp half <*> genExp half]
    where
        half = (size - 1) `div` 2

-- quickCheck prop_integerAddAssoc
prop_integerAddAssoc :: Integer -> Integer -> Integer -> Bool
prop_integerAddAssoc n1 n2 n3 = (n1 + n2) + n3 == n1 + (n2 + n3)

-- quickCheck prop_aplAddAssoc
prop_aplAddAssoc :: Exp -> Exp -> Exp -> Bool
prop_aplAddAssoc e1 e2 e3 = runEval (eval (Add (Add e1 e2) e3)) == runEval (eval (Add e1 (Add e2 e3)))