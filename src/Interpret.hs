{-# LANGUAGE LambdaCase #-}
module Interpret where

import Machine (Machine (..))
import qualified Machine as M
import Types
import Prelude hiding (drop)

add :: Machine -> Machine
add m = M.apply2 (\x y -> case (x, y) of
    (Int x', Int y')     -> Right $ Int (x' + y')
    (Float x', Float y') -> Right $ Float (x' + y')
    (x, y)               -> Left $ M.err ("Cannot perform `+` on " ++ show x ++ " and " ++ show y) m) m

sub :: Machine -> Machine
sub m = M.apply2 (\x y -> case (x, y) of
    (Int x', Int y')     -> Right $ Int (y' - x')
    (Float x', Float y') -> Right $ Float (y' - x')
    (x, y)               -> Left $ M.err ("Cannot perform `-` on " ++ show x ++ " and " ++ show y) m) m

mul :: Machine -> Machine
mul m = M.apply2 (\x y -> case (x, y) of
    (Int x', Int y')     -> Right $ Int (x' * y')
    (Float x', Float y') -> Right $ Float (x' * y')
    (x, y)               -> Left $ M.err ("Cannot perform `*` on " ++ show x ++ " and " ++ show y) m) m

div :: Machine -> Machine
div m = M.apply2 (\x y -> case (x, y) of
    (Int x', Int y')     -> Right $ Int (Prelude.div y' x')
    (Float x', Float y') -> Right $ Float (y' / x')
    (x, y)               -> Left $ M.err ("Cannot perform `/` on " ++ show x ++ " and " ++ show y) m) m

mod :: Machine -> Machine
mod m = M.apply2 (\x y -> case (x, y) of
    (Int x', Int y')     -> Right $ Int (Prelude.mod y' x')
    (x, y)               -> Left $ M.err ("Cannot perform `%` on " ++ show x ++ " and " ++ show y) m) m

exp :: Machine -> Machine
exp m = M.apply2 (\x y -> case (x, y) of
    (Int x', Int y')     -> Right $ Int (y' ^ x')
    (Float x', Float y') -> Right $ Float (y' ** x')
    (x, y)               -> Left $ M.err ("Cannot perform `^` on " ++ show x ++ " and " ++ show y) m) m

eq :: Machine -> Machine
eq m = M.apply2 (\x y -> case (x, y) of
    (Int x', Int y')       -> Right $ Bool (x' == y')
    (Float x', Float y')   -> Right $ Bool (x' == y')
    (Bool x', Bool y')     -> Right $ Bool (x' == y')
    (String x', String y') -> Right $ Bool (x' == y')
    (x, y)                 -> Left $ M.err ("Cannot perform `==` on " ++ show x ++ " and " ++ show y) m) m

gt :: Machine -> Machine
gt m = M.apply2 (\x y -> case (x, y) of
    (Int x', Int y')       -> Right $ Bool (x' > y')
    (Float x', Float y')   -> Right $ Bool (x' > y')
    (x, y)                 -> Left $ M.err ("Cannot perform `>` on " ++ show x ++ " and " ++ show y) m) m

lt :: Machine -> Machine
lt m = M.apply2 (\x y -> case (x, y) of
    (Int x', Int y')       -> Right $ Bool (x' < y')
    (Float x', Float y')   -> Right $ Bool (x' < y')
    (x, y)                 -> Left $ M.err ("Cannot perform `<` on " ++ show x ++ " and " ++ show y) m) m

ge :: Machine -> Machine
ge m = M.apply2 (\x y -> case (x, y) of
    (Int x', Int y')       -> Right $ Bool (x' >= y')
    (Float x', Float y')   -> Right $ Bool (x' >= y')
    (x, y)                 -> Left $ M.err ("Cannot perform `>=` on " ++ show x ++ " and " ++ show y) m) m

le :: Machine -> Machine
le m = M.apply2 (\x y -> case (x, y) of
    (Int x', Int y')       -> Right $ Bool (x' <= y')
    (Float x', Float y')   -> Right $ Bool (x' <= y')
    (x, y)                 -> Left $ M.err ("Cannot perform `<=` on " ++ show x ++ " and " ++ show y) m) m

ne :: Machine -> Machine
ne m = M.apply2 (\x y -> case (x, y) of
    (Int x', Int y')       -> Right $ Bool (x' /= y')
    (Float x', Float y')   -> Right $ Bool (x' /= y')
    (Bool x', Bool y')     -> Right $ Bool (x' /= y')
    (String x', String y') -> Right $ Bool (x' /= y')
    (x, y)                 -> Left $ M.err ("Cannot perform `!=` on " ++ show x ++ " and " ++ show y) m) m

or :: Machine -> Machine
or m = M.apply2 (\x y -> case (x, y) of
    (Bool x', Bool y')     -> Right $ Bool (x' || y')
    (x, y)                 -> Left $ M.err ("Cannot perform `|` on " ++ show x ++ " and " ++ show y) m) m

and :: Machine -> Machine
and m = M.apply2 (\x y -> case (x, y) of
    (Bool x', Bool y')     -> Right $ Bool (x' && y')
    (x, y)                 -> Left $ M.err ("Cannot perform `&` on " ++ show x ++ " and " ++ show y) m) m

not :: Machine -> Machine
not m = M.apply (\case
    Bool x' -> Right $ Bool (Prelude.not x')
    x       -> Left $ M.err ("Cannot perform `!` on " ++ show x) m) m

truth :: Machine -> (Maybe Bool, Machine)
truth m = case M.pop m of
    (Just x, m') -> case x of
        Bool x' -> (Just x', m')
        _ -> (Nothing, M.err "Invalid type for `truth`" m)
    _ -> (Nothing, M.err "Invalid type for `truth`" m)

r = return

evalExpr :: Expr -> Machine -> IO Machine
evalExpr e m = case e of
    Push lv -> r $ M.push (val lv) m
    Call s -> do
        let f = M.getFunc (val s) m
        case f of
            Just body -> evalExprs body m
            Nothing   -> do
                let mv = M.get (val s) m
                case mv of
                    Nothing -> r $ M.err ("Function not found: " ++ val s) m
                    Just va -> r $ M.push va m
    Intr s -> case val s of
        "+"  -> r $ add m
        "-"  -> r $ sub m
        "*"  -> r $ mul m
        "/"  -> r $ Interpret.div m
        "%"  -> r $ Interpret.mod m
        "^"  -> r $ Interpret.exp m
        "="  -> r $ eq m
        ">"  -> r $ gt m
        "<"  -> r $ lt m
        ">=" -> r $ ge m
        "<=" -> r $ le m
        "!=" -> r $ ne m
        "||" -> r $ Interpret.or m
        "&&" -> r $ Interpret.and m
        "!"  -> r $ Interpret.not m
        "?"  -> do
            print $ stack m
            return m
        "dup"    -> r $ M.apply (\x -> Left $ m { stack = x : stack m }) m
        "drop"   -> r $ M.drop 1 m
        "swap"   -> r $ M.apply2 (\x y -> Left $ m { stack = y : x : stack (M.drop 2 m) }) m
        "over"   -> r $ M.apply2 (\x y -> Left $ m { stack = x : y : x : stack (M.drop 2 m) }) m
        "rot"    -> r $ M.apply3 (\x y z -> Left $ m { stack = z : x : y : stack (M.drop 3 m) }) m
        "puts"   -> r $ M.apply (\x -> Left $ M.out (display x) (M.drop 1 m)) m
        "putsln" -> r $ M.apply (\x -> Left $ M.out (display x ++ "\n") (M.drop 1 m)) m
        "gets"   -> getLine >>= \x -> r $ M.push (String x) m
        "flush"  -> M.putBuf m
        "void"   -> r m
        -- Probably will never happen because
        -- parser should've caught it
        _ -> error "unreachable"
    If t f -> do
        let (b, m') = truth m
        if b == Just True
        then evalExprs (map val t) m'
        else evalExprs (map val f) m'
    Try t o -> do
        let iom = evalExprs (map val t) m
        iom >>= \m' -> if fault m'
            then evalExprs (map val o) m
            else return m'
    Take vs body -> do
        let len = length vs
        if len > length (stack m) then
            r $ M.err ("Not enough element on the stack (have " ++ show (length (stack m)) ++ " but need " ++ show len ++ ")") m
        else do
            let elems = reverse $ take len (stack m)
            let m' = M.bindl (zip (map val vs) elems) (M.drop len m)
            evalExprs (map val body) m'
    Peek vs body -> do
        let len = length vs
        if len > length (stack m) then
            r $ M.err ("Not enough element on the stack (have " ++ show (length (stack m)) ++ " but need " ++ show len ++ ")") m
        else do
            let elems = reverse $ take len (stack m)
            let m' = M.bindl (zip (map val vs) elems) m
            evalExprs (map val body) m'

evalExprs :: [Expr] -> Machine -> IO Machine
evalExprs [] m = return m
evalExprs (e:es) m = do
    m' <- evalExpr e m
    if fault m' then return m' else evalExprs es m'

evalStmt :: Stmt -> Machine -> IO Machine
evalStmt s m = case s of
    Entry body -> evalExprs (map val body) m
    Func name _ _ body -> r $ M.bindFunc name (map val body) m

evalProgram :: Program -> Machine -> IO Machine
evalProgram [] m = return m
evalProgram (s:ss) m = do
    m' <- evalStmt (val s) m
    if fault m' then return m' else evalProgram ss m'
