module Interpreter where

import GHC.Base (when)
import Types
import qualified Control.Monad.Trans        as T
import qualified Control.Monad.Trans.Except as E
import qualified Control.Monad.Trans.State  as S
import qualified Data.Map                   as Map

-- | Main interpreter data types
data Interpreter = Interpreter
    { stack :: [Value]
    , bufio :: [String]
    , env   :: Map.Map String Value
    , funcs :: Map.Map String ([Type], [Type], [Expression])
    } deriving (Show)

initInterpreter :: Interpreter
initInterpreter = Interpreter [] [] Map.empty Map.empty

type InterpreterT a = E.ExceptT String (S.StateT Interpreter IO) a

-- | Stack based operations for the interpreter

try :: InterpreterT a -> InterpreterT a
try f = do
    s <- T.lift S.get
    f `E.catchE` (\e -> T.lift (S.put s) >> E.throwE e)

require :: String -> Int -> InterpreterT ()
require f n = T.lift S.get >>= \i -> if length (stack i) >= n
    then return ()
    else E.throwE $ "not enough element on the stack for " ++ f ++ ""

push :: Value -> InterpreterT ()
push x = T.lift S.get >>= \i -> T.lift $ S.put i { stack = x : stack i }

pop :: InterpreterT Value
pop = T.lift S.get >>= \i -> case stack i of
        [] -> E.throwE "stack underflow while popping"
        (v:vs) -> do
            T.lift $ S.put i { stack = vs }
            return v

peek :: InterpreterT Value
peek = T.lift S.get >>= \i -> case stack i of
        [] -> E.throwE "stack underflow while peeking"
        (v:_) -> return v

popn :: Int -> InterpreterT [Value]
popn n = T.lift S.get >>= \i -> case splitAt n (stack i) of
        ([], _) -> E.throwE "stack underflow while popping multiple elements"
        (vs, _) -> do
            T.lift $ S.put i { stack = drop n (stack i) }
            return vs

-- | IO operations

putbuf :: String -> InterpreterT ()
putbuf s = T.lift S.get >>= \i -> T.lift $ S.put i { bufio = s : bufio i }

flushbuf :: InterpreterT ()
flushbuf = T.lift S.get >>= \i -> do
    T.liftIO $ putStr $ concat $ reverse $ bufio i
    T.lift $ S.put i { bufio = [] }

-- | Variable and environment bindings

envset :: String -> Value -> InterpreterT ()
envset s v = T.lift S.get >>= \i -> T.lift $ S.put i { env = Map.insert s v (env i) }

envsets :: [(String, Value)] -> InterpreterT ()
envsets sv = T.lift S.get >>= \i -> T.lift $ S.put i { env = Map.union (env i) (Map.fromList sv) }

envunset :: String -> InterpreterT ()
envunset s = T.lift S.get >>= \i -> T.lift $ S.put i { env = Map.delete s (env i) }

envunsets :: [String] -> InterpreterT ()
envunsets ss = T.lift S.get >>= \i -> T.lift $ S.put i { env = Map.difference (env i) (Map.fromList $ zip ss (repeat ())) }

envget :: String -> InterpreterT Value
envget s = T.lift S.get >>= \i -> case Map.lookup s (env i) of
    Just v -> return v
    Nothing -> E.throwE $ "undefined variable: " ++ s

funcset :: Statement -> InterpreterT ()
funcset (Function name args ret body) =
        T.lift S.get >>= \i -> T.lift $ S.put i { funcs = Map.insert name (args, ret, body) (funcs i) }
funcset _ = error "unreachable"

funcget :: String -> InterpreterT [Expression]
funcget s = T.lift S.get >>= \i -> case Map.lookup s (funcs i) of
    Just (_, _, body) -> return body
    Nothing -> E.throwE $ "undefined function: " ++ s

funcexist :: String -> InterpreterT Bool
funcexist s = T.lift S.get >>= \i -> case Map.lookup s (funcs i) of
    Just _ -> return True
    Nothing -> return False

eval :: Expression -> InterpreterT ()
eval (Push x) = push x
eval (Call n) = case n of

    -- Stack-related functions --

    "dup" -> do
        require n 1
        pop >>= \x -> push x >> push x
        return ()
    "drop" -> do
        require n 1
        _ <- pop
        return ()
    "swap" -> do
        require n 2
        pop >>= \a -> pop >>= \b -> push a >> push b
    "over" -> do
        require n 2
        pop >>= \a -> pop >>= \b -> push a >> push b >> push a
    "rot" -> do
        require n 3
        pop >>= \a -> pop >>= \b -> pop >>= \c -> push b >> push a >> push c
    "apply" -> do
        require n 2
        pop >>= \x -> if isq x then evals (unq x) else E.throwE "not a function"
        where isq (ValueQuote _) = True
              isq _              = False
              unq (ValueQuote q) = q
              unq _              = error "unreachable"

    -- Arithmetic functions --

    "+" -> do
        require n 2
        pop >>= \x -> pop >>= \y -> case (x, y) of
            (ValueInt x', ValueInt y') -> push $ ValueInt (x' + y')
            (ValueFloat x', ValueFloat y') -> push $ ValueFloat (x' + y')
            _ -> E.throwE "+: type error"
    "-" -> do
        require n 2
        pop >>= \x -> pop >>= \y -> case (x, y) of
            (ValueInt x', ValueInt y') -> push $ ValueInt (y' - x')
            (ValueFloat x', ValueFloat y') -> push $ ValueFloat (y' - x')
            _ -> E.throwE "-: type error"
    "*" -> do
        require n 2
        pop >>= \x -> pop >>= \y -> case (x, y) of
            (ValueInt x', ValueInt y') -> push $ ValueInt (x' * y')
            (ValueFloat x', ValueFloat y') -> push $ ValueFloat (x' * y')
            _ -> E.throwE "*: type error"
    "/" -> do
        require n 2
        pop >>= \x -> pop >>= \y -> case (x, y) of
            (ValueInt x', ValueInt y') -> push $ ValueInt (y' `div` x')
            (ValueFloat x', ValueFloat y') -> push $ ValueFloat (y' / x')
            _ -> E.throwE "/: type error"
    "%" -> do
        require n 2
        pop >>= \x -> pop >>= \y -> case (x, y) of
            (ValueInt x', ValueInt y') -> push $ ValueInt (y' `mod` x')
            _ -> E.throwE "%: type error"
    "=" -> require n 2 >> pop >>= \x -> pop >>= \y -> push $ ValueBool (x == y)
    "<" -> require n 2 >> pop >>= \x -> pop >>= \y -> push $ ValueBool (y < x)
    ">" -> require n 2 >> pop >>= \x -> pop >>= \y -> push $ ValueBool (y > x)

    -- List & conversions functions --

    "collect" -> require n 1 >> pop >>= \x -> case x of
        ValueInt amount -> require n amount >> popn amount >>= push . ValueList
        _ -> undefined
    "join" -> require n 2 >> pop >>= \x -> pop >>= \y -> case (x, y) of
        (ValueList a, ValueList b) -> push $ ValueList (a ++ b)
        _ -> E.throwE "join: type error"
    "unjoin" -> require n 1 >> pop >>= \x -> case x of
        ValueList a -> mapM_ push (reverse a)
        _ -> E.throwE "unjoin: type error"
    "head" -> require n 1 >> pop >>= \x -> case x of
        ValueList [] -> E.throwE "head: empty list"
        ValueList (x:_) -> push x
        _ -> E.throwE "head: type error"
    "tail" -> require n 1 >> pop >>= \x -> case x of
        ValueList [] -> E.throwE "head: empty list"
        ValueList (_:xs) -> push $ ValueList xs
        _ -> E.throwE "tail: type error"
    "index" -> require n 2 >> pop >>= \x -> pop >>= \y -> case (x, y) of
        (ValueInt a, ValueList xs) -> if length xs <= a then
            E.throwE "index: out of bounds"
            else push $ xs !! a
        _ -> E.throwE "index: type error"
    "tostr"  -> require n 1 >> pop >>= \x -> push $ (unstr . show) x

    -- IO functions --

    "debug"   -> T.lift S.get >>= \i -> putbuf (show (stack i) ++ "\n") >> flushbuf
    "gets"    -> T.liftIO getLine >>= \l -> push $ ValueList $ map ValueChar l
    "flush"   -> flushbuf
    -- Destructive printing
    "puts"    -> require n 1 >> pop >>= putbuf . show
    "putsln"  -> require n 1 >> pop >>= \x -> putbuf (show x ++ "\n")
    -- Non-destructive printing
    "sputs"   -> require n 1 >> peek >>= putbuf . show
    "sputsln" -> require n 1 >> peek >>= \x -> putbuf (show x ++ "\n")
    _ -> funcexist n >>= \x -> if x then funcget n >>= evals
        else envget n >>= push
eval (Quote q) = push $ ValueQuote q
eval (If t f) = pop >>= \x -> if x == ValueBool True
    then evals t
    else evals f
eval (Try t c) = do
    try (evals t) `E.catchE` (\err -> do
        push (unstr err) -- Push the error message to the stack
        evals c)
eval (Take ns f) = do
    let len = length ns
    "take" `require` len >> popn len >>=
        \xs -> envsets (zip ns xs) >> evals f >> envunsets ns
eval (While c f) = evals c >> pop >>= \x -> when (x == ValueBool True) $ evals f >> eval (While c f)
eval (Bind name) = "binding" `require` 1 >> pop >>= \x -> envset name x

evals :: [Expression] -> InterpreterT ()
evals = mapM_ eval

evalStmt :: Statement -> InterpreterT ()
evalStmt (Use _) = return ()
evalStmt (Function name args ret body) = do
    funcset (Function name args ret body)
    when (name == "main") $ evals body

evalStmts :: [Statement] -> InterpreterT ()
evalStmts = mapM_ evalStmt