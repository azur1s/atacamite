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
    _ -> Left $ M.err "Invalid types for `+`" m) m

sub :: Machine -> Machine
sub m = M.apply2 (\x y -> case (x, y) of
    (Int x', Int y')     -> Right $ Int (y' - x')
    (Float x', Float y') -> Right $ Float (y' - x')
    _ -> Left $ M.err "Invalid types for `-`" m) m

mul :: Machine -> Machine
mul m = M.apply2 (\x y -> case (x, y) of
    (Int x', Int y')     -> Right $ Int (x' * y')
    (Float x', Float y') -> Right $ Float (x' * y')
    _ -> Left $ M.err "Invalid types for `*`" m) m

div :: Machine -> Machine
div m = M.apply2 (\x y -> case (x, y) of
    (Int x', Int y')     -> Right $ Int (Prelude.div y' x')
    (Float x', Float y') -> Right $ Float (y' / x')
    _ -> Left $ M.err "Invalid types for `/`" m) m

mod :: Machine -> Machine
mod m = M.apply2 (\x y -> case (x, y) of
    (Int x', Int y')     -> Right $ Int (Prelude.mod y' x')
    _ -> Left $ M.err "Invalid types for `%`" m) m

exp :: Machine -> Machine
exp m = M.apply2 (\x y -> case (x, y) of
    (Int x', Int y')     -> Right $ Int (y' ^ x')
    (Float x', Float y') -> Right $ Float (y' ** x')
    _ -> Left $ M.err "Invalid types for `^`" m) m

eq :: Machine -> Machine
eq m = M.apply2 (\x y -> case (x, y) of
    (Int x', Int y')       -> Right $ Bool (x' == y')
    (Float x', Float y')   -> Right $ Bool (x' == y')
    (Bool x', Bool y')     -> Right $ Bool (x' == y')
    (String x', String y') -> Right $ Bool (x' == y')
    _ -> Left $ M.err "Invalid types for `==`" m) m

gt :: Machine -> Machine
gt m = M.apply2 (\x y -> case (x, y) of
    (Int x', Int y')       -> Right $ Bool (x' > y')
    (Float x', Float y')   -> Right $ Bool (x' > y')
    _ -> Left $ M.err "Invalid types for `>`" m) m

lt :: Machine -> Machine
lt m = M.apply2 (\x y -> case (x, y) of
    (Int x', Int y')       -> Right $ Bool (x' < y')
    (Float x', Float y')   -> Right $ Bool (x' < y')
    _ -> Left $ M.err "Invalid types for `<`" m) m

ge :: Machine -> Machine
ge m = M.apply2 (\x y -> case (x, y) of
    (Int x', Int y')       -> Right $ Bool (x' >= y')
    (Float x', Float y')   -> Right $ Bool (x' >= y')
    _ -> Left $ M.err "Invalid types for `>=`" m) m

le :: Machine -> Machine
le m = M.apply2 (\x y -> case (x, y) of
    (Int x', Int y')       -> Right $ Bool (x' <= y')
    (Float x', Float y')   -> Right $ Bool (x' <= y')
    _ -> Left $ M.err "Invalid types for `<=`" m) m

ne :: Machine -> Machine
ne m = M.apply2 (\x y -> case (x, y) of
    (Int x', Int y')       -> Right $ Bool (x' /= y')
    (Float x', Float y')   -> Right $ Bool (x' /= y')
    (Bool x', Bool y')     -> Right $ Bool (x' /= y')
    (String x', String y') -> Right $ Bool (x' /= y')
    _ -> Left $ M.err "Invalid types for `!=`" m) m

or :: Machine -> Machine
or m = M.apply2 (\x y -> case (x, y) of
    (Bool x', Bool y')     -> Right $ Bool (x' || y')
    _ -> Left $ M.err "Invalid types for `||`" m) m

and :: Machine -> Machine
and m = M.apply2 (\x y -> case (x, y) of
    (Bool x', Bool y')     -> Right $ Bool (x' && y')
    _ -> Left $ M.err "Invalid types for `&&`" m) m

not :: Machine -> Machine
not m = M.apply (\case
    Bool x' -> Right $ Bool (Prelude.not x')
    _ -> Left $ M.err "Invalid types for `!`" m) m

drop :: Machine -> Machine
drop m = snd $ M.pop m

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
            Nothing   -> return $ M.err ("Function not found: " ++ val s) m
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
        "drop"   -> r $ drop m
        "swap"   -> r $ M.apply2 (\x y -> Left $ m { stack = y : x : stack (drop $ drop m) }) m
        "over"   -> r $ M.apply2 (\x y -> Left $ m { stack = x : y : x : stack (drop $ drop m) }) m
        "rot"    -> r $ M.apply3 (\x y z -> Left $ m { stack = z : x : y : stack (drop $ drop $ drop m) }) m
        "puts"   -> r $ M.apply (\x -> Left $ M.out (display x) (drop m)) m
        "putsln" -> r $ M.apply (\x -> Left $ M.out (display x ++ "\n") (drop m)) m
        "gets"   -> getLine >>= \x -> r $ M.push (String x) m
        "flush"  -> M.putBuf m
        -- Probably will never happen because
        -- parser should've caught it
        _ -> r $ M.err ("Unknown intrinsic: `" ++ val s ++ "`") m
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
