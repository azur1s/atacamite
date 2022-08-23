{-# LANGUAGE OverloadedStrings #-}
module Parser where

import Data.Foldable (toList)
import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, unpack)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Types                      as T

convSP :: SourcePos -> (String, Int, Int)
convSP (SourcePos f l c) = (f, unPos l, unPos c)

type Parser = Parsec Void Text

-- | Basic parsers

-- Space consumer
sc :: Parser ()
sc = L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

int :: Parser Int
int = lexeme L.decimal

float :: Parser Float
float = lexeme L.float

stringl :: Parser String
stringl = lexeme (char '\"' *> manyTill L.charLiteral (char '\"'))

charl :: Parser Char
charl = lexeme (char '\'' *> L.charLiteral <* char '\'')

reserved :: [String]
reserved = ["if", "else", "then", "try", "catch", "end", "true", "false"]

keyword :: Text -> Parser Text
keyword k = lexeme (string k <* notFollowedBy alphaNumChar)

ident :: Parser String
ident = lexeme . try $ do
    s <- (:) <$> oneOf first <*> many (oneOf rest)
    if s `elem` reserved
        then fail $ "reserved keyword: " ++ show s
        else return s
    where
        first = ['a'..'z'] ++ ['A'..'Z'] ++ "!@#$%^&*-_=+|<>/?"
        rest = first ++ ['0'..'9']

tyident :: Parser String
tyident = lexeme $ (:) <$> oneOf first <*> many (oneOf rest)
    where
        first = ['a'..'z']
        rest = first ++ ['A'..'Z']

-- | Value parsers

valueInt :: Parser T.Value
valueInt = T.ValueInt <$> int

valueFloat :: Parser T.Value
valueFloat = T.ValueFloat <$> float

valueBool :: Parser T.Value
valueBool = T.ValueBool <$> (keyword "true" $> True <|> keyword "false" $> False)

valueChar :: Parser T.Value
valueChar = T.ValueChar <$> charl

valueString :: Parser T.Value
valueString = T.unstr <$> stringl

valueList :: Parser T.Value
valueList = T.ValueList <$> between (symbol "[") (symbol "]") (many value)

value :: Parser T.Value
value = valueInt <|> valueFloat <|> valueBool <|> valueChar <|> valueString <|> valueList
    <?> "literals"

-- | Expression

push :: Parser T.Expression
push = T.Push <$> value

call :: Parser T.Expression
call = T.Call <$> ident <?> "identifier"

quote :: Parser T.Expression
quote = T.Quote <$> between (symbol "(") (symbol ")") (many expr)
    <?> "quote expression"

ifelse :: Parser T.Expression
ifelse = T.If <$ keyword "if" <*> many expr
    <* keyword "else" <*> many expr <* keyword "then"
    <?> "if expression"

tryelse :: Parser T.Expression
tryelse = T.Try <$ keyword "try" <*> many expr
    <* keyword "catch" <*> many expr <* keyword "end"
    <?> "try expression"

bind :: Parser T.Expression
bind = T.Bind <$> (symbol "->" *> ident) <?> "bind expression"

expr :: Parser T.Expression
expr = ifelse <|> tryelse <|> quote <|> bind <|> call <|> push
    <?> "expression"

-- | Typehint parsers

ty :: Parser T.Type
ty = T.TypeInt         <$ keyword "Int"
    <|> T.TypeFloat    <$ keyword "Float"
    <|> T.TypeBool     <$ keyword "Bool"
    <|> T.TypeChar     <$ keyword "Char"
    <|> T.TypeList     <$> (symbol "[" *> ty <* symbol "]")
    <|> T.TypeGeneric  <$> tyident

-- | Statement parsers

function :: Parser T.Statement
function = do
    name <- symbol ":" *> ident
    args <- symbol "(" *> sepBy ty (symbol ",")
    rets <- optional (symbol "--" *> sepBy ty (symbol ","))
    _    <- symbol ")"
    body <- some expr <* symbol "."
    return $ T.Function name args (fromMaybe [] rets) body
    <?> "function statement"

stmt :: Parser T.Statement
stmt = function <?> "statement"

parseAll :: String -> String -> Either (ParseErrorBundle Text Void) [T.Statement]
parseAll path source = parse (sc *> many stmt <* eof <?> "statements") path (pack source)

errorUnpack :: ParseErrorBundle Text Void -> NonEmpty (SourcePos, Text)
errorUnpack peb = fmap (\(err, pos) -> (pos, pack . parseErrorTextPretty $ err)) . fst $
    attachSourcePos errorOffset (bundleErrors peb) (bundlePosState peb)

fmtErrors :: NonEmpty (SourcePos, Text) -> [String]
fmtErrors = fmap
    (\(pos, msg) -> sourceName pos ++ ":"
                ++ (s . sourceLine) pos ++ ":"
                ++ (s . sourceColumn) pos ++ " "
                ++ (unln . unpack) msg)
    . toList
    where
        s = show . unPos
        unln ""        = ""
        unln ('\n':xs) = ',':' ':unln xs
        unln (x:xs)    = x:unln xs