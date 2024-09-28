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
    parseTest,
    satisfy,
    some,
    try,
  )
import Text.Megaparsec.Char (space)

-- Do not change this definition.
type Parser = Parsec Void String


lString :: String -> Parser ()
lString s = lexeme $ void $ chunk s

-- Atom ::= var
--        | int
--        | bool
--        | "(" Exp ")"
pAtom :: Parser Exp
pAtom = choice
          [ Var <$> lVName,
            CstInt <$> lInteger,
            CstBool <$> pBool,
            lString "(" *> pExp <* lString ")"
          ]

-- LExp ::= "if" Exp "then" Exp "else" Exp
pLExp :: Parser Exp
pLExp =
  choice
    [ If
        <$> (lKeyword "if" *> pExp0)
        <*> (lKeyword "then" *> pExp0)
        <*> (lKeyword "else" *> pExp0),
      pAtom
    ]


-- Exp1' ::=            (* empty *)
--         | "*" Atom Exp1'
--         | "/" Atom Exp1'

-- Exp1 ::= Atom Exp1'
pExp1 :: Parser Exp
pExp1 = pLExp >>= chain
  where
    chain x =
      choice
        [ do
            lString "*"
            y <- pLExp
            chain $ Mul x y,
          do
            lString "/"
            y <- pLExp
            chain $ Div x y,
          pure x
        ]

-- Exp0' ::=            (* empty *)
--         | "+" Exp1 Exp0'
--         | "-" Exp1 Exp0'

-- Exp0 ::= Exp1 Exp0'
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

-- Exp  ::= Exp0
pExp :: Parser Exp
pExp = pExp0


-- Do not change this definition.
parseAPL :: FilePath -> String -> Either String Exp
parseAPL fname s = case parse (space *> pExp <* eof) fname s of
  Left err -> Left $ errorBundlePretty err
  Right x -> Right x


keywords :: [String]
keywords = ["if", "then", "else", "true", "false", "let", "in", "try", "catch", "print", "put", "get"]


lexeme :: Parser a -> Parser a
lexeme p = p <* space

-- digit = "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9";
-- int = digit {digit};
-- Exp ::= int "+" int;
lInteger :: Parser Integer
lInteger = lexeme $ read <$> some (satisfy isDigit) <* notFollowedBy (satisfy isAlpha)

-- alphabetic = ? any alphabetic character ?;
-- alphanumeric = ? any alphanumeric character ?;
-- var = alphabetic {alphanumeric};
lVName :: Parser VName
lVName = lexeme $ try $ do
  c <- satisfy isAlpha
  cs <- many $ satisfy isAlphaNum
  let v = c:cs
  if v `elem` keywords
    then fail "Unexpected keyword"
    else pure v

lKeyword :: String -> Parser ()
lKeyword s = lexeme $ void $ try $ chunk s <* notFollowedBy (satisfy isAlphaNum)

-- bool ::= "true" | "false";
pBool :: Parser Bool
pBool = 
  choice 
    [ const True <$> lKeyword "true",
      const False <$> lKeyword "false"
    ]

