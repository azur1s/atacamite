{-# LANGUAGE OverloadedStrings #-}
module Parser where

import Data.Either (isLeft, isRight)
import Data.Foldable (toList)
import Data.Functor (($>))
import Data.List (isPrefixOf)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, unpack)
import Data.Void (Void)
import System.Directory (canonicalizePath, setCurrentDirectory, getHomeDirectory)
import System.FilePath (takeDirectory, (</>))
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
    (L.skipLineComment "\\")
    (L.skipBlockComment "(*" "*)")

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
reserved = ["if", "else", "then", "try", "catch", "while", "do", "end", "true", "false"]

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
value = try valueFloat <|> valueInt <|> valueBool <|> valueChar <|> valueString <|> valueList
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
    <* keyword "else" <*> many expr <* keyword "end"
    <?> "if expression"

tryelse :: Parser T.Expression
tryelse = T.Try <$ keyword "try" <*> many expr
    <* keyword "catch" <*> many expr <* keyword "end"
    <?> "try expression"

takeblock :: Parser T.Expression
takeblock = T.Take <$ keyword "take"
    <* symbol "(" <*> some ident <* symbol ")"
    <*> many expr <* keyword "end"
    <?> "take expression"

while :: Parser T.Expression
while = T.While <$ keyword "while" <*> some expr
    <* keyword "do" <*> many expr <* symbol "end"
    <?> "while expression"

bind :: Parser T.Expression
bind = T.Bind <$> (symbol "->" *> ident) <?> "bind expression"

expr :: Parser T.Expression
expr = ifelse <|> tryelse <|> takeblock <|> while
    <|> quote <|> bind
    <|> call <|> push
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

use :: Parser T.Statement
use = T.Use <$ keyword "use" <*> stringl <?> "use statement"

function :: Parser T.Statement
function = do
    name <- symbol ":" *> ident
    _    <- symbol "("
    args <- optional (sepBy ty (symbol ","))
    rets <- optional (symbol "->" *> sepBy ty (symbol ","))
    _    <- symbol ")"
    body <- some expr <* symbol ";"
    return $ T.Function name (fromMaybe [] args) (fromMaybe [] rets) body
    <?> "function statement"

stmt :: Parser T.Statement
stmt = use <|> function <?> "statement"

parseAll :: String -> String -> Either (ParseErrorBundle Text Void) [T.Statement]
parseAll path source = parse (sc *> many stmt <* eof <?> "statements") path (pack source)

-- | Miscellaneous functions

parseFile :: FilePath -> IO (Either (ParseErrorBundle Text Void) [T.Statement])
parseFile path = do
    fullPath <- canonicalizePath path
    setCurrentDirectory $ takeDirectory fullPath

    contents <- readFile fullPath
    case parseAll path (contents ++ "\n") of
        Left err -> return $ Left err
        Right p -> do
            let imports = getImports p

            home <- getHomeDirectory
            let coreFull = map
                    (\c -> home </> ".atacamite" </> drop 4 c)  -- ~/.atacamite/..
                    (filter (isPrefixOf "std/") imports) -- use std/..
            let paths' = coreFull ++ filter (not . isPrefixOf "std/") imports

            progs' <- mapM canonicalizePath paths' >>= \x -> mapM parseFile x
            let errs = filter isLeft progs'
            if null errs then do
                let ps = map (\(Right p) -> p) (filter isRight progs')
                let all = concat ps ++ p
                return $ Right all
                else return $ Left $ head (map (\(Left e) -> e) errs)
    where
        getImports = map i . f
            where
                i = \(T.Use path) -> path ++ ".ata"
                f = filter (\s -> case s of T.Use _ -> True; _ -> False)

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