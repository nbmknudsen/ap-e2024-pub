module APL.Tests
  ( properties
  )
where

import APL.AST (Exp (..), VName, subExp, printExp)
import APL.Error (isVariableError, isDomainError, isTypeError)
import APL.Check (checkExp)
import APL.Parser (parseAPL, keywords)
import APL.Eval (runEval, eval)
import Test.QuickCheck
  ( Property
  , Gen
  , Arbitrary (arbitrary, shrink)
  , property
  , cover
  , checkCoverage
  , oneof
  , sized
  , frequency
  , elements
  , choose
  , collect
  )

instance Arbitrary Exp where
  arbitrary = sized (\n -> genExp n [])
    
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
    e1 : e2 : [If cond' e1   e2 | cond' <- shrink cond] ++ [If cond e1' e2 | e1' <- shrink e1] ++ [If cond e1 e2' | e2' <- shrink e2]
  shrink (Let x e1 e2) =
    e1 : [Let x e1' e2 | e1' <- shrink e1] ++ [Let x e1 e2' | e2' <- shrink e2]
  shrink (Lambda x e) =
    [Lambda x e' | e' <- shrink e]
  shrink (Apply e1 e2) =
    e1 : e2 : [Apply e1' e2 | e1' <- shrink e1] ++ [Apply e1 e2' | e2' <- shrink e2]
  shrink (TryCatch e1 e2) =
    e1 : e2 : [TryCatch e1' e2 | e1' <- shrink e1] ++ [TryCatch e1 e2' | e2' <- shrink e2]
  shrink _ = []


genExp :: Int -> [VName] -> Gen Exp
genExp 0 _ = oneof [CstInt <$> arbitrary, CstBool <$> arbitrary]
genExp size vars =
  frequency
    [ (2, CstInt <$> arbitrary)
    , (2, CstBool <$> arbitrary)
    , (2, Add <$> genExp halfSize vars <*> genExp halfSize vars)
    , (2, Sub <$> genExp halfSize vars <*> genExp halfSize vars)
    , (2, Mul <$> genExp halfSize vars <*> genExp halfSize vars)
    , (5, Div <$> genExp halfSize vars <*> genExp halfSize vars)
    , (5, Pow <$> genExp halfSize vars <*> genExp halfSize vars)
    , (2, Eql <$> genExp halfSize vars <*> genExp halfSize vars)
    , (1, If <$> genExp thirdSize vars <*> genExp thirdSize vars <*> genExp thirdSize vars)
    , (2, Var <$> randomVarName)
    , (20, Let <$> randomVname <*> genExp halfSize vars <*> genExp halfSize vars)
    , (40, Lambda <$> randomVname <*> genExp (size - 1) vars)
    , (1, Apply <$> genExp halfSize vars <*> genExp halfSize vars) 
    , (30, TryCatch <$> genExp halfSize vars <*> genExp halfSize vars)
    ]
  where
    halfSize = size `div` 2
    thirdSize = size `div` 3
    randomVarName = do
      len <- choose (2, 4) :: Gen Int
      varName <- mapM (\_ -> elements ['a'..'z']) [1..len]
      if varName `elem` keywords
        then randomVarName
        else return varName
    randomVname = do
      len <- choose (1, 10) :: Gen Int
      -- First character must be an alphabetic letter (uppercase or lowercase)
      firstChar <- elements (['a'..'z'] ++ ['A'..'Z'])
      -- Remaining characters can be either letters (lowercase/uppercase) or digits
      restChars <- mapM (\_ -> elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])) [1..(len - 1)]
      let varName = firstChar : restChars
      if varName `elem` keywords
        then randomVname
        else return varName

expCoverage :: Exp -> Property        
expCoverage e = checkCoverage
  . cover 20 (any isDomainError (checkExp e)) "domain error"
  . cover 20 (not $ any isDomainError (checkExp e)) "no domain error"
  . cover 20 (any isTypeError (checkExp e)) "type error"
  . cover 20 (not $ any isTypeError (checkExp e)) "no type error"
  . cover 5 (any isVariableError (checkExp e)) "variable error"
  . cover 70 (not $ any isVariableError (checkExp e)) "no variable error"
  . cover 50 (or [2 <= n && n <= 4 | Var v <- subExp e, let n = length v]) "non-trivial variable"
  $ ()


parsePrinted :: Exp -> Property
parsePrinted e = collect e $
  printParseCheck e

printParseCheck :: Exp -> Bool
printParseCheck e1 =
  case parseAPL "" (printExp e1) of
    Left _ -> False 
    Right parsedExp -> e1 == parsedExp


onlyCheckedErrors :: Exp -> Property
onlyCheckedErrors e =
  case runEval (eval e) of
    Left err ->
      property (err `elem` checkExp e)
    Right _ ->
      property True


properties :: [(String, Property)]
properties =
  [("expCoverage", property expCoverage)
  , ("onlyCheckedErrors", property onlyCheckedErrors)
  , ("parsePrinted", property parsePrinted)
  ]
