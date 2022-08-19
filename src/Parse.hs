{-# LANGUAGE OverloadedStrings #-}

module Parse where

import Data.Functor (($>), (<&>))
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, unpack)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Foldable (toList)

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
    deriving (Show, Eq, Ord)

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
    | If    Body Body
    | Try   Body String Body
    | Take  [String] Body
    | Peek  [String] Body
    | While Body Body
    | Store String
    | Load  String
    deriving (Show)

type Body = [Expr]

data Hint
    = HInt
    | HFloat
    | HBool
    | HString
    | HList Hint
    | HGen String
    deriving (Show)

data Stmt
    = Import String
    | Const String (Locatable Atom)
    | Fun  String [Locatable Hint] [Locatable Hint] Body
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
        first = ['a'..'z'] ++ ['A'..'Z'] ++ "_" ++ "!@#$%^&*_+-=,./<>?|"
        rest = first ++ ['0'..'9'] ++ "'"

hident :: Parser String
hident = lexeme $ (:) <$> oneOf first <*> many (oneOf rest)
    where
        first = ['a'..'z']
        rest = first ++ ['A'..'Z']

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
intrList = [ "+", "-", "*", "/", "%", "^"
    , "=", "<", ">", "<=", ">=", "!="
    , "||", "&&", "!"
    , "?"
    , "@", "!.", "..", "!!", "//", "id", ".*", "rev"
    , "dup", "drop", "swap", "over", "rot"
    , "puts", "putsln"
    , "gets", "flush", "sleep"
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
    <*> ( symbol "}" *> keyword "else" *> ident) <*> (symbol "{" *> exprs <* symbol "}") <?> "try block"

takeblk :: Parser Expr
takeblk = Take <$> (keyword "take" *> some ident)
    <*> (symbol "{" *> exprs <* symbol "}") <?> "take block"

peekblk :: Parser Expr
peekblk = Peek <$> (keyword "peek" *> some ident)
    <*> (symbol "{" *> exprs <* symbol "}") <?> "peek block"

while :: Parser Expr
while = While <$> (keyword "while" *> exprs)
    <*> (symbol "{" *> exprs <* symbol "}") <?> "while block"

store :: Parser Expr
store = Store <$> (keyword "store" *> symbol "(" *> ident <* symbol ")") <?> "store"

load :: Parser Expr
load = Load <$> (keyword "load" *> symbol "(" *> ident <* symbol ")") <?> "load"

expr :: Parser Expr
expr = ifelse <|> tryelse <|> takeblk <|> peekblk <|> while <|> push <|> store <|> load <|> callintr <?> "expression"

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

hint :: Parser (Locatable Hint)
hint = do
    s <- getSourcePos
    Locatable (convSP s) <$> (
        HInt        <$ keyword "Int"
        <|> HFloat  <$ keyword "Float"
        <|> HBool   <$ keyword "Bool"
        <|> HString <$ keyword "String"
        <|> HList   <$> ((symbol "[" *> hint <* symbol "]") <&> value)
        <|> HGen    <$> hident
        <?> "typehint")

hints' :: Parser [Locatable Hint]
hints' = many hint <?> "typehints"

-- | Statements

importf :: Parser Stmt
importf = Import <$> (keyword "import" *> ident) <?> "import"

consta :: Parser Stmt
consta = Const <$> (keyword "const" *> ident) <*> atom <?> "constant"

fun :: Parser Stmt
fun = do
    name <- keyword "fun" *> ident
    args <- hints'
    rets <- optional (keyword "--" *> hints')
    body <- symbol "{" *> exprs <* symbol "}"
    return $ Fun name args (fromMaybe [] rets) body

entry :: Parser Stmt
entry = Entry <$> (keyword "entry" *> symbol "{" *> exprs <* symbol "}") <?> "entry"

stmt :: Parser Stmt
stmt = fun <|> entry <|> importf <?> "statement"

stmt' :: Parser (Locatable Stmt)
stmt' = do
    s <- getSourcePos
    Locatable (convSP s) <$> (consta <|> fun <|> entry <|> importf <?> "statement")

stmts' :: Parser Program
stmts' = many stmt' <?> "statements"

parseProgram :: String -> String -> Either (ParseErrorBundle Text Void) Program
parseProgram path source = parse (sc *> stmts' <* eof <?> "statements") path (pack source)

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