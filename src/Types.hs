module Types where

data Value
    = ValueInt   Int
    | ValueFloat Float
    | ValueBool  Bool
    | ValueChar  Char
    | ValueList  [Value]
    | ValueQuote [Expression]
    deriving (Eq, Ord)

variant :: Value -> Value -> Bool
variant (ValueInt   _) (ValueInt   _) = True
variant (ValueFloat _) (ValueFloat _) = True
variant (ValueBool  _) (ValueBool  _) = True
variant (ValueChar  _) (ValueChar  _) = True
variant (ValueList  _) (ValueList  _) = True
variant (ValueQuote _) (ValueQuote _) = True
variant _ _ = False

sameVariant :: [Value] -> Bool
sameVariant [] = True
sameVariant (x:xs) = all (variant x) xs

unstr :: String -> Value
unstr = ValueList . map ValueChar

instance Show Value where
    show (ValueInt i)   = show i
    show (ValueFloat f) = show f
    show (ValueBool b)  = show b
    show (ValueChar c)  = show c
    show (ValueList l)  =
        if sameVariant l
        then case head l of
            ValueChar _ -> map (\(ValueChar c) -> c) l
            _           -> "[" ++ unwords (map show l) ++ "]"
        else "[" ++ unwords (map show l) ++ "]"
    show (ValueQuote q) = "(" ++ unwords (map show q) ++ ")"

data Expression
    = Push Value
    | Call String
    | Quote [Expression]
    | If [Expression] [Expression] [Expression]
    | Try [Expression] [Expression]
    | Bind String
    deriving (Show, Eq, Ord)

data Type
    = TypeInt | TypeFloat | TypeBool | TypeChar | TypeList Type
    | TypeGeneric String
    | TypeQuote   [Type] [Type]
    deriving (Show)

data Statement
    = Function String [Type] [Type] [Expression]
    deriving (Show)