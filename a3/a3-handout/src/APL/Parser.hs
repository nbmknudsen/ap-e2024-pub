module APL.Parser (parseAPL) where

import APL.AST (Exp (..), VName)
import Control.Monad (void)
import Data.Char (isAlpha, isAlphaNum, isDigit)
import Data.Void (Void)
import Text.Megaparsec
  ( Parsec,
    choice,
    chunk,
    eof,
    errorBundlePretty,
    many,
    notFollowedBy,
    parse,
    satisfy,
    some,
    try,
    noneOf,
  )
import Text.Megaparsec.Char (space, char)

type Parser = Parsec Void String

lexeme :: Parser a -> Parser a
lexeme p = p <* space

keywords :: [String]
keywords =
  [ "if",
    "then",
    "else",
    "true",
    "false",
    "let",
    "in",
    "try",
    "catch",
    "print",
    "put",
    "get"
  ]

lVName :: Parser VName
lVName = lexeme $ try $ do
  c <- satisfy isAlpha
  cs <- many $ satisfy isAlphaNum
  let v = c : cs
  if v `elem` keywords
    then fail "Unexpected keyword"
    else pure v

lInteger :: Parser Integer
lInteger =
  lexeme $ read <$> some (satisfy isDigit) <* notFollowedBy (satisfy isAlphaNum)

lString :: String -> Parser ()
lString s = lexeme $ void $ chunk s

lKeyword :: String -> Parser ()
lKeyword s = lexeme $ void $ try $ chunk s <* notFollowedBy (satisfy isAlphaNum)

lBool :: Parser Bool
lBool =
  lexeme . try . choice $
    [ const True <$> lKeyword "true",
      const False <$> lKeyword "false"
    ]

pAtom :: Parser Exp
pAtom =
  choice
    [ CstInt <$> lInteger,
      CstBool <$> lBool,
      Var <$> lVName,
      lString "(" *> pExp <* lString ")"
    ]
    
pStringParser :: Parser String
pStringParser = char '"' *> many (noneOf "\"") <* char '"'

pFExp :: Parser Exp
pFExp = do
          first <- pAtom
          rest <- many pAtom
          return (foldl Apply first rest)


pLExp :: Parser Exp
pLExp =
  choice
    [ If
        <$> (lKeyword "if" *> pExp)
        <*> (lKeyword "then" *> pExp)
        <*> (lKeyword "else" *> pExp),
        do
          _ <- lKeyword "\\"
          varName <- lVName
          _ <- lKeyword "->"
          bodyExp <- pExp
          return (Lambda varName bodyExp),
        do
          _ <- lKeyword "try"
          tryExp <- pExp
          _ <- lKeyword "catch"
          catchExp <- pExp
          return (TryCatch tryExp catchExp),
        do
          _ <- lKeyword "let"
          varName <- lVName 
          _ <- lKeyword "="
          letExp <- pExp
          _ <- lKeyword "in"
          inExp <- pExp
          return (Let varName letExp inExp),
      pFExp
    ]
pExp3 :: Parser Exp
pExp3 = 
  choice
    [
      do
        lKeyword "print"
        str <- lexeme pStringParser
        atom <- pAtom
        return (Print str atom),
      do
        lKeyword "get"
        atom <- pAtom
        return (KvGet atom),
      do
        lKeyword "put"
        atom1 <- pAtom
        atom2 <- pAtom
        return (KvPut atom1 atom2),
      pLExp
    ]

pExp2 :: Parser Exp
pExp2 = pExp3 >>= chain
  where
    chain x =
      choice
        [ do
            lString "**"
            y <- pExp2
            chain $ Pow x y,
          pure x
        ]

pExp1 :: Parser Exp
pExp1 = pExp2 >>= chain
  where
    chain x =
      choice
        [ do
            lString "*"
            y <- pExp2
            chain $ Mul x y,
          do
            lString "/"
            y <- pExp2
            chain $ Div x y,
          pure x
        ]

pExp0 :: Parser Exp
pExp0 = pExp1 >>= chain
  where
    chain x =
      choice
        [ do
            lString "+"
            y <- pExp1
            chain $ Add x y,
          do
            lString "-"
            y <- pExp1
            chain $ Sub x y,
          pure x
        ]

pExpN1 :: Parser Exp
pExpN1 = do
  first <- pExp0
  rest <- many (lString "==" *> pExp0)
  return $ foldl Eql first rest

pExp :: Parser Exp
pExp = pExpN1

parseAPL :: FilePath -> String -> Either String Exp
parseAPL fname s = case parse (space *> pExp <* eof) fname s of
  Left err -> Left $ errorBundlePretty err
  Right x -> Right x