module Parse where

import Control.Monad (void)
import qualified Text.Parsec        as P
import qualified Text.Parsec.String as P
import Types

ws :: P.Parser ()
ws = void (P.oneOf " \t\r\n")

comment :: P.Parser ()
comment = void (P.char ';' >> P.skipMany (P.noneOf "\r\n") >> P.char '\n')

noise :: P.Parser ()
noise = P.skipMany (ws P.<|> comment)

identifier :: P.Parser String
identifier = P.many1 (P.letter P.<|> P.oneOf "+-*/%^=<>!|&?_~@.,()")

reserved :: [String]
reserved =
    [ "+", "-", "*", "/", "%", "^"
    , "=", "<", ">", "<=", ">=", "!="
    , "|", "&", "!"
    , "?"
    , "dup", "drop", "swap", "over", "rot"
    , "puts", "putsln"
    , "gets", "flush"
    ]

-- | Values

unit :: P.Parser (Locatable Value)
unit = do
    start <- P.getPosition
    _ <- P.string "()"
    end <- P.getPosition
    return $ Locatable Unit (start, end)

int :: P.Parser (Locatable Value)
int = do
    start <- P.getPosition
    n <- P.many1 P.digit
    end <- P.getPosition
    return $ Locatable (Int (read n)) (start, end)

float :: P.Parser (Locatable Value)
float = do
    start <- P.getPosition
    n <- P.many1 P.digit
    _ <- P.char '.'
    d <- P.many1 P.digit
    end <- P.getPosition
    return $ Locatable (Float (read (n ++ "." ++ d))) (start, end)

bool :: P.Parser (Locatable Value)
bool = do
    start <- P.getPosition
    _ <- P.char '#'
    b <- P.string "true" P.<|> P.string "false"
    end <- P.getPosition
    return $ Locatable (Bool (b == "true")) (start, end)

string :: P.Parser (Locatable Value)
string = do
    start <- P.getPosition
    s <- P.between (P.char '"') (P.char '"') (P.many $ P.noneOf "\"" P.<|> P.try (P.string "\"\"" >> return '"'))
    end <- P.getPosition
    return $ Locatable (String s) (start, end)

array :: P.Parser (Locatable Value)
array = do
    start <- P.getPosition
    _ <- P.char '[' >> noise
    e <- P.sepBy (P.choice [int, float, bool, string]) (noise >> P.char ',' >> noise)
    _ <- P.char ']' >> noise
    end <- P.getPosition
    return $ Locatable (Array e) (start, end)

-- | Expressions

push :: P.Parser (Locatable Expr)
push = do
    start <- P.getPosition
    v <- P.choice [int, float, bool, string, array]
    end <- P.getPosition
    return $ Locatable (Push v) (start, end)

callintr :: P.Parser (Locatable Expr)
callintr = do
    start <- P.getPosition
    c <- identifier
    end <- P.getPosition
    if c `elem` reserved
        then return $ Locatable (Intr c) (start, end)
        else return $ Locatable (Call c) (start, end)

ifelse :: P.Parser (Locatable Expr)
ifelse = do
    start <- P.getPosition
    _ <- P.string "if" >> noise >> P.string "{" >> noise
    t <- P.many1 (noise *> expr <* noise)
    _ <- noise >> P.string "}" >> noise >> P.string "else" >> noise >> P.string "{" >> noise
    f <- P.many1 (noise *> expr <* noise)
    _ <- noise >> P.string "}" >> noise
    end <- P.getPosition
    return $ Locatable (If t f) (start, end)

try :: P.Parser (Locatable Expr)
try = do
    start <- P.getPosition
    _ <- P.string "try" >> noise >> P.string "{" >> noise
    t <- P.many1 (noise *> expr <* noise)
    _ <- noise >> P.string "}" >> noise >> P.string "otherwise" >> noise >> P.string "{" >> noise
    o <- P.many1 (noise *> expr <* noise)
    _ <- noise >> P.string "}" >> noise
    end <- P.getPosition
    return $ Locatable (Try t o) (start, end)

expr :: P.Parser (Locatable Expr)
expr = P.choice [ifelse, try, push, callintr] P.<?> "expression"

-- | Statements

func :: P.Parser (Locatable Stmt)
func = do
    start <- P.getPosition
    _ <- P.string "func" >> noise
    name <- identifier
    _ <- noise >> P.string "--" >> noise
    tys <- P.many (noise *> identifier <* noise) P.<?> "argument types"
    _ <- P.string ":"
    ret <- noise *> identifier <* noise P.<?> "return type"
    _ <- P.string "{"
    body <- P.many1 (noise *> expr <* noise)
    _ <- noise >> P.string "}" >> noise
    end <- P.getPosition
    return $ Locatable (Func name tys ret body) (start, end)

entry :: P.Parser (Locatable Stmt)
entry = do
    start <- P.getPosition
    P.string "entry" >> noise >> P.string "{" >> noise
    body <- P.many1 (noise *> expr <* noise)
    noise >> P.string "}" >> noise
    end <- P.getPosition
    return $ Locatable (Entry body) (start, end)

program :: P.Parser Program
program = do
    P.many1 (noise *> P.choice [func, entry] <* noise) P.<?> "program"

parseProgram :: String -> String -> Either P.ParseError Program
parseProgram = P.parse program