{-# LANGUAGE OverloadedStrings #-}

module Parse where

import Data.Functor (($>))
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, unpack)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

data Locatable a = Locatable { location :: (String, Int, Int), value :: a }
    deriving (Show)

convSP :: SourcePos -> (String, Int, Int)
convSP (SourcePos f l c) = (f, unPos l, unPos c)

data Atom
    = AInt    Int
    | AFloat  Float
    | ABool   Bool
    | AString String
    | AList   [Atom]
    deriving (Show, Eq)

printAtom :: Atom -> String
printAtom a = case a of
    AInt i    -> show i
    AFloat f  -> show f
    ABool b   -> show b
    AString s -> s
    AList l   -> "[" ++ intercalate ", " (map printAtom l) ++ "]"

data Expr
    = Push (Locatable Atom)
    | Call (Locatable String)
    | Intr (Locatable String)
    | If   Body Body
    | Try  Body Body
    | Take [String] Body
    | Peek [String] Body
    deriving (Show)

type Body = [Expr]

data Hint
    = HInt
    | HFloat
    | HBool
    | HString
    | HList Hint
    deriving (Show)

data Stmt
    = Func  String [Locatable Hint] [Locatable Hint] Body
    | Entry Body
    deriving (Show)

type Program = [Locatable Stmt]

type Parser = Parsec Void Text

-- Space consumer
sc :: Parser ()
sc = L.space
    space1
    (L.skipLineComment ";;")
    (L.skipBlockComment ";*" "*;")

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

keyword :: Text -> Parser Text
keyword k = lexeme (string k <* notFollowedBy alphaNumChar)

ident :: Parser String
ident = lexeme $ (:) <$> oneOf first <*> many (oneOf rest)
    where
        first = ['a'..'z'] ++ ['A'..'Z'] ++ "_" ++ "!@#$%^&*_+-=,./<>?:"
        rest = first ++ ['0'..'9'] ++ "'"

-- | Atoms

aInt :: Parser Atom
aInt = AInt <$> int

aFloat :: Parser Atom
aFloat = AFloat <$> float

aBool :: Parser Atom
aBool = ABool <$> (keyword "true" $> True <|> keyword "false" $> False)

aString :: Parser Atom
aString = AString <$> stringl

aList :: Parser Atom
aList = AList <$> between
    (symbol "[") (symbol "]") (sepBy (aInt <|> aFloat <|> aBool <|> aString <|> aList) (symbol ","))

atom :: Parser (Locatable Atom)
atom = do
    s <- getSourcePos
    Locatable (convSP s) <$> (aInt <|> aFloat <|> aBool <|> aString <|> aList)

atoms :: Parser [Locatable Atom]
atoms = some atom

parseAtoms :: String -> String -> Either (ParseErrorBundle Text Void) [Locatable Atom]
parseAtoms path source = parse (atoms <* eof <?> "atoms") path (pack source)

-- | Expressions

push :: Parser Expr
push = Push <$> atom

intrList :: [String]
intrList = [ "+", "-", "*", "/", "%"
    , "=", "<", ">", "<=", ">=", "!="
    , "||", "&&", "!"
    , "?"
    , "dup", "drop", "swap", "over", "rot"
    , "puts", "putsln"
    , "gets", "flush"
    ]

callintr :: Parser Expr
callintr = do
    s <- getSourcePos
    i <- ident
    if i `elem` intrList
        then return $ Intr $ Locatable (convSP s) i
        else return $ Call $ Locatable (convSP s) i

ifelse :: Parser Expr
ifelse = If <$> (keyword "if" *> symbol "{" *> exprs)
    <*> ( symbol "}" *> keyword "else" *> symbol "{" *> exprs <* symbol "}") <?> "if block"

tryelse :: Parser Expr
tryelse = Try <$> (keyword "try" *> symbol "{" *> exprs)
    <*> ( symbol "}" *> keyword "else" *> symbol "{" *> exprs <* symbol "}") <?> "try block"

takeblk :: Parser Expr
takeblk = Take <$> (keyword "take" *> some ident)
    <*> (symbol "{" *> exprs <* symbol "}") <?> "take block"

peekblk :: Parser Expr
peekblk = Peek <$> (keyword "peek" *> some ident)
    <*> (symbol "{" *> exprs <* symbol "}") <?> "peek block"

expr :: Parser Expr
expr = ifelse <|> tryelse <|> takeblk <|> peekblk <|> push <|> callintr <?> "expression"

exprs :: Parser Body
exprs = many expr <?> "body"

expr' :: Parser (Locatable Expr)
expr' = do
    s <- getSourcePos
    Locatable (convSP s) <$> (ifelse <|> tryelse <|> takeblk <|> peekblk <|> callintr <|> push <?> "expression")

exprs' :: Parser [Locatable Expr]
exprs' = some expr' <?> "body"

parseExprs :: String -> String -> Either (ParseErrorBundle Text Void) [Locatable Expr]
parseExprs path source = parse (exprs' <* eof <?> "expression") path (pack source)

-- | Hint

hint :: Parser Hint
hint = 
    HInt     <$ keyword "int"
    <|> HFloat  <$ keyword "float"
    <|> HBool   <$ keyword "bool"
    <|> HString <$ keyword "string"
    <|> HList   <$> (symbol "[" *> hint <* symbol "]")
    <?> "typehint"

hint' :: Parser (Locatable Hint)
hint' = do
    s <- getSourcePos
    Locatable (convSP s) <$> (
        HInt        <$ keyword "int"
        <|> HFloat  <$ keyword "float"
        <|> HBool   <$ keyword "bool"
        <|> HString <$ keyword "string"
        <|> HList   <$> (symbol "[" *> hint <* symbol "]")
        <?> "typehint")

hints' :: Parser [Locatable Hint]
hints' = many hint' <?> "typehints"

-- | Statements

func :: Parser Stmt
-- func name a b c , d e f { .. }
func = do
    id <- keyword "func" *> ident
    args <- hints'
    rets <- optional (keyword "--" *> hints')
    body <- symbol "{" *> exprs <* symbol "}"
    return $ Func id args (fromMaybe [] rets) body

entry :: Parser Stmt
entry = Entry <$> (keyword "entry" *> symbol "{" *> exprs <* symbol "}") <?> "entry"

stmt :: Parser Stmt
stmt = func <|> entry <?> "statement"

stmt' :: Parser (Locatable Stmt)
stmt' = do
    s <- getSourcePos
    Locatable (convSP s) <$> (func <|> entry <?> "statement")

stmts' :: Parser Program
stmts' = some stmt' <?> "statements"

parseProgram :: String -> String -> Either (ParseErrorBundle Text Void) Program
parseProgram path source = parse (stmts' <* eof <?> "statements") path (pack source)