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

pExp :: Parser Exp
pExp = choice
          [ CstInt <$> lInteger,
            CstBool <$> pBool,
            Var <$> lVName
          ]


-- Do not change this definition.
parseAPL :: FilePath -> String -> Either String Exp
parseAPL fname s = case parse (space *> pExp <* eof) fname s of
  Left err -> Left $ errorBundlePretty err
  Right x -> Right x




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
lVName = lexeme $ do
  c <- satisfy isAlpha
  cs <- many $ satisfy isAlphaNum
  pure $ c:cs

lKeyword :: String -> Parser ()
lKeyword s = lexeme $ void $ try $ chunk s <* notFollowedBy (satisfy isAlphaNum)

-- bool ::= "true" | "false";
pBool :: Parser Bool
pBool = 
  choice 
    [ const True <$> lKeyword "true",
      const False <$> lKeyword "false"
    ]
