module Types where

import Text.Parsec (SourcePos, sourceName, sourceLine, sourceColumn)
import Data.List (intercalate)

data Locatable a = Locatable
    { val :: a
    , loc :: (SourcePos, SourcePos)
    } deriving (Show)

data Value
    = Unit
    | Int    Int
    | Float  Float
    | Bool   Bool
    | String String
    | Array  [Locatable Value]
    deriving (Show)

data Expr
    = Push (Locatable Value)
    | Call String
    | Intr String -- Intrinsic
    | If   [Locatable Expr] [Locatable Expr]
    | Try  [Locatable Expr] [Locatable Expr]
    deriving (Show)

data Stmt
    = Func String [String] String [Locatable Expr]
    | Entry [Locatable Expr]
    deriving (Show)

type Program = [Locatable Stmt]

-- | Debug printing

indent :: String -> String
indent s = replicate 4 ' ' ++ s

nindent :: String -> String
nindent s = "\n" ++ indent s

reset :: String -> String
reset s = "\x1b[0m" ++ s

grey :: String -> String
grey s = "\x1b[90m" ++ s ++ reset ""

red :: String -> String
red s = "\x1b[91m" ++ s ++ reset ""

green :: String -> String
green s = "\x1b[92m" ++ s ++ reset ""

yellow :: String -> String
yellow s = "\x1b[93m" ++ s ++ reset ""

blue :: String -> String
blue s = "\x1b[94m" ++ s ++ reset ""

fmtLoc :: (SourcePos, SourcePos) -> String
fmtLoc (start, end) =
    grey $ sourceName start ++ "@"
    ++ show (sourceLine start) ++ ":" ++ show (sourceColumn start) ++ "~"
    ++ show (sourceLine end)   ++ ":" ++ show (sourceColumn end)
    ++ reset ""

fmtValue :: Locatable Value -> String
fmtValue (Locatable v _) = fmt v
    where
        fmt Unit = "()"
        fmt (Int i) = show i
        fmt (Float f) = show f
        fmt (Bool b) = show b
        fmt (String s) = show s
        fmt (Array a) = "[" ++ intercalate ", " (map fmtValue a) ++ "]"

fmtExpr :: Locatable Expr -> String
fmtExpr (Locatable e l) = case e of
    Push v -> yellow $ fmtValue v ++ " " ++ fmtLoc l
    Call s -> s ++ " " ++ fmtLoc l
    Intr s -> red s ++ " " ++ fmtLoc l
    If t f -> blue "if {\n"
        ++ indent (intercalate (nindent "") $ map fmtExpr t)
        ++ nindent (blue "} else {\n")
        ++ indent (intercalate (nindent "") $ map fmtExpr f)
        ++ nindent (blue "}")
    Try t o -> blue "try {\n"
        ++ indent (intercalate (nindent "") $ map fmtExpr t)
        ++ nindent (blue "} catch {\n")
        ++ indent (intercalate (nindent "") $ map fmtExpr o)
        ++ nindent (blue "}")

fmtStmt :: Locatable Stmt -> String
fmtStmt (Locatable s l) = fmt s ++ " " ++ fmtLoc l
    where
        fmt (Func name args ret body) = yellow "func" ++ " ( "
            ++ "name: " ++ green name
            ++ "; args: " ++ green (intercalate ", " args)
            ++ "; return: " ++ green ret
            ++ " ) " ++ yellow "{\n" ++ indent (intercalate (nindent "") $ map fmtExpr body) ++ yellow "\n}"
        fmt (Entry body) = yellow "entry {\n" ++ indent (intercalate (nindent "") $ map fmtExpr body) ++ yellow "\n}"

fmtProgram :: Program -> String
fmtProgram = intercalate "\n" . map fmtStmt