{-# LANGUAGE LambdaCase #-}
module Interpreter where

import GHC.Base (when)
import Types
import qualified Control.Monad.Trans        as T
import qualified Control.Monad.Trans.Except as E
import qualified Control.Monad.Trans.State  as S
import qualified Data.Map                   as Map

data Interpreter = Interpreter
    { stack :: [Value]
    , bufio :: [String]
    , env   :: Map.Map String Value
    , funcs :: Map.Map String ([Type], [Type], [Expression])
    } deriving (Show)

initInterpreter :: Interpreter
initInterpreter = Interpreter [] [] Map.empty Map.empty

type InterpreterT a = E.ExceptT String (S.StateT Interpreter IO) a

try :: InterpreterT a -> InterpreterT a
try f = do
    s <- T.lift S.get
    f `E.catchE` (\e -> T.lift (S.put s) >> E.throwE e)

pop :: InterpreterT Value
pop = T.lift S.get >>= \i -> case stack i of
        [] -> E.throwE "stack underflow"
        (v:vs) -> do
            T.lift $ S.put i { stack = vs }
            return v

popn :: Int -> InterpreterT [Value]
popn n = T.lift S.get >>= \i -> case splitAt n (stack i) of
        ([], _) -> E.throwE "stack underflow"
        (vs, _) -> do
            T.lift $ S.put i { stack = drop n (stack i) }
            return vs

putbuf :: String -> InterpreterT ()
putbuf s = T.lift S.get >>= \i -> T.lift $ S.put i { bufio = s : bufio i }

flushbuf :: InterpreterT ()
flushbuf = T.lift S.get >>= \i -> do
    T.liftIO $ putStr $ concat $ reverse $ bufio i
    T.lift $ S.put i { bufio = [] }

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

funcget :: String -> InterpreterT [Expression]
funcget s = T.lift S.get >>= \i -> case Map.lookup s (funcs i) of
    Just (_, _, body) -> return body
    Nothing -> E.throwE $ "undefined function: " ++ s

funcexist :: String -> InterpreterT Bool
funcexist s = T.lift S.get >>= \i -> case Map.lookup s (funcs i) of
    Just _ -> return True
    Nothing -> return False

push :: Value -> InterpreterT ()
push x = T.lift S.get >>= \i -> T.lift $ S.put i { stack = x : stack i }

require :: String -> Int -> InterpreterT ()
require f n = T.lift S.get >>= \i -> if length (stack i) >= n
    then return ()
    else E.throwE $ "not enough element on the stack for " ++ f ++ ""

eval :: Expression -> InterpreterT ()
eval (Push x) = push x
eval (Call n) = case n of
    -- Stack
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
        pop >>= \a -> pop >>= \b -> pop >>= \c -> push b >> push c >> push a
    "apply" -> do
        require n 2
        pop >>= \x -> if isq x then evals (unq x) else E.throwE "not a function"
        where isq (ValueQuote _) = True
              isq _              = False
              unq (ValueQuote q) = q
              unq _              = error "unreachable"
    -- Math
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
    -- List & conversions
    "collect" -> require n 1 >> pop >>= \case
        ValueInt amount -> require n amount >> popn amount >>= push . ValueList
        _ -> undefined
    "join" -> require n 2 >> pop >>= \x -> pop >>= \y -> case (x, y) of
        (ValueList a, ValueList b) -> push $ ValueList (a ++ b)
        _ -> E.throwE "join: type error"
    "head" -> require n 1 >> pop >>= \case
        ValueList [] -> E.throwE "head: empty list"
        ValueList (x:_) -> push x
        _ -> E.throwE "head: type error"
    "tail" -> require n 1 >> pop >>= \case
        ValueList [] -> E.throwE "head: empty list"
        ValueList (_:xs) -> push $ ValueList xs
        _ -> E.throwE "tail: type error"
    "index" -> require n 2 >> pop >>= \x -> pop >>= \y -> case (x, y) of
        (ValueInt a, ValueList xs) -> if length xs <= a then
            E.throwE "index: out of bounds"
            else push $ xs !! a
        _ -> E.throwE "index: type error"
    "tostr"  -> require n 1 >> pop >>= \x -> push $ (unstr . show) x
    -- IO
    "debug"  -> T.lift S.get >>= \i -> putbuf (show (stack i) ++ "\n") >> flushbuf
    "puts"   -> require n 1 >> pop >>= putbuf . show
    "putsln" -> require n 1 >> pop >>= \x -> putbuf (show x ++ "\n")
    "gets"   -> T.liftIO getLine >>= \l -> push $ ValueList $ map ValueChar l
    "flush"  -> flushbuf
    _ -> funcexist n >>= \x -> if x then funcget n >>= evals
        else envget n >>= push
eval (Quote q) = push $ ValueQuote q
eval (If t f) = pop >>= \x -> if x == ValueBool True
    then evals t
    else evals f
eval (Try t c) = do
    try (evals t) `E.catchE` (\err -> do
        push (unstr err)
        evals c)
eval (Take ns f) = do
    let len = length ns
    "take" `require` len >> popn len >>=
        \xs -> envsets (zip ns xs) >> evals f >> envunsets ns
eval (While c f) = do
    evals c >> pop >>= \x -> when (x == ValueBool True) $ evals f >> eval (While c f)
eval (Bind name) = "binding" `require` 1 >> pop >>= \x -> envset name x

evals :: [Expression] -> InterpreterT ()
evals = mapM_ eval

evalStmt :: Statement -> InterpreterT ()
evalStmt (Function name args ret body) = do
    funcset (Function name args ret body)
    when (name == "main") $ evals body

evalStmts :: [Statement] -> InterpreterT ()
evalStmts = mapM_ evalStmt