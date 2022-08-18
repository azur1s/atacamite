module Interpret where

import Machine (Machine (..))
import Parse (Expr (..), Locatable (..), Stmt(..), Program, Atom (..), printAtom)
import qualified Machine as M

check :: Machine -> (Maybe Atom, Machine)
check m = if fault c then (Nothing, c) else do
    let (a, m')  = M.popUnsafe c
    (Just a, m')
    where c = M.require 1 m

check2 :: Machine -> (Maybe (Atom, Atom), Machine)
check2 m = if fault c then (Nothing, c) else do
    let (a, m')  = M.popUnsafe c
    let (b, m'') = M.popUnsafe m'
    (Just (a, b), m'')
    where c = M.require 2 m

binopErr :: [Char] -> Atom -> Atom -> Machine -> Machine
binopErr op a b = M.err ("Cannot perform `" ++ op ++ "` on " ++ show a ++ " and " ++ show b)

unaopErr :: [Char] -> Atom -> Machine -> Machine
unaopErr op a = M.err ("Cannot perform `" ++ op ++ "` on " ++ show a)

add, sub, mul, div, mod, exp :: Machine -> Machine
eq, gt, lt, ge, le, ne, or, and, not :: Machine -> Machine

add m = case check2 m of
    (Just r, m') -> case r of
        (AInt a, AInt b)     -> M.push (AInt (a + b)) m'
        (AFloat a, AFloat b) -> M.push (AFloat (a + b)) m'
        (a, b) -> binopErr "+" a b m'
    (Nothing, m') -> error "unreachable"

sub m = case check2 m of
    (Just r, m') -> case r of
        (AInt a, AInt b)     -> M.push (AInt (b - a)) m'
        (AFloat a, AFloat b) -> M.push (AFloat (b - a)) m'
        (a, b) -> binopErr "-" a b m'
    (Nothing, m') -> error "unreachable"

mul m = case check2 m of
    (Just r, m') -> case r of
        (AInt a, AInt b)     -> M.push (AInt (a * b)) m'
        (AFloat a, AFloat b) -> M.push (AFloat (a * b)) m'
        (a, b) -> binopErr "*" a b m'
    (Nothing, m') -> error "unreachable"

div m = case check2 m of
    (Just r, m') -> case r of
        (AInt a, AInt b)     -> M.push (AInt (Prelude.div b a)) m'
        (AFloat a, AFloat b) -> M.push (AFloat (b / a)) m'
        (a, b) -> binopErr "/" a b m'
    (Nothing, m') -> error "unreachable"

mod m = case check2 m of
    (Just r, m') -> case r of
        (AInt a, AInt b)     -> M.push (AInt (Prelude.mod b a)) m'
        (a, b) -> binopErr "%" a b m'
    (Nothing, m') -> error "unreachable"

exp m = case check2 m of
    (Just r, m') -> case r of
        (AInt a, AInt b)     -> M.push (AInt (b ^ a)) m'
        (AFloat a, AFloat b) -> M.push (AFloat (b ** a)) m'
        (a, b) -> binopErr "^" a b m'
    (Nothing, m') -> error "unreachable"

eq m = case check2 m of
    (Just r, m') -> case r of
        (AInt a, AInt b)       -> M.push (ABool (a == b)) m'
        (AFloat a, AFloat b)   -> M.push (ABool (a == b)) m'
        (ABool a, ABool b)     -> M.push (ABool (a == b)) m'
        (AString a, AString b) -> M.push (ABool (a == b)) m'
        (AList a, AList b)     -> M.push (ABool (a == b)) m'
        (a, b) -> binopErr "=" a b m'
    (Nothing, m') -> error "unreachable"

gt m = case check2 m of
    (Just r, m') -> case r of
        (AInt a, AInt b)       -> M.push (ABool (a > b)) m'
        (AFloat a, AFloat b)   -> M.push (ABool (a > b)) m'
        (a, b) -> binopErr ">" a b m'
    (Nothing, m') -> error "unreachable"

lt m = case check2 m of
    (Just r, m') -> case r of
        (AInt a, AInt b)       -> M.push (ABool (a < b)) m'
        (AFloat a, AFloat b)   -> M.push (ABool (a < b)) m'
        (a, b) -> binopErr "<" a b m'
    (Nothing, m') -> error "unreachable"

ge m = case check2 m of
    (Just r, m') -> case r of
        (AInt a, AInt b)       -> M.push (ABool (a >= b)) m'
        (AFloat a, AFloat b)   -> M.push (ABool (a >= b)) m'
        (a, b) -> binopErr ">=" a b m'
    (Nothing, m') -> error "unreachable"

le m = case check2 m of
    (Just r, m') -> case r of
        (AInt a, AInt b)       -> M.push (ABool (a <= b)) m'
        (AFloat a, AFloat b)   -> M.push (ABool (a <= b)) m'
        (a, b) -> binopErr "<=" a b m'
    (Nothing, m') -> error "unreachable"

ne m = case check2 m of
    (Just r, m') -> case r of
        (AInt a, AInt b)       -> M.push (ABool (a /= b)) m'
        (AFloat a, AFloat b)   -> M.push (ABool (a /= b)) m'
        (a, b) -> binopErr "!=" a b m'
    (Nothing, m') -> error "unreachable"

or m = case check2 m of
    (Just r, m') -> case r of
        (ABool a, ABool b) -> M.push (ABool (a || b)) m'
        (a, b) -> binopErr "or" a b m'
    (Nothing, m') -> error "unreachable"

and m = case check2 m of
    (Just r, m') -> case r of
        (ABool a, ABool b) -> M.push (ABool (a && b)) m'
        (a, b) -> binopErr "and" a b m'
    (Nothing, m') -> error "unreachable"

not m = case check m of
    (Just r, m') -> case r of
        (ABool a) -> M.push (ABool (Prelude.not a)) m'
        a -> unaopErr "not" a m'
    (Nothing, m') -> error "unreachable"

evalExpr :: Expr -> Machine -> IO Machine
evalExpr e m = case e of
    Push lv -> return $ M.push (value lv) m
    Call s  -> do
        let f = M.getFunc (value s) m
        case f of
            Just body -> evalExprs body m
            Nothing   -> do
                let mv = M.get (value s) m
                case mv of
                    Nothing -> return $ M.err ("Function not found: " ++ value s) m
                    Just va -> return $ M.push va m
    Intr s -> case value s of
        "+"  -> return $ add m
        "-"  -> return $ sub m
        "*"  -> return $ mul m
        "/"  -> return $ Interpret.div m
        "%"  -> return $ Interpret.mod m
        "^"  -> return $ Interpret.exp m
        "="  -> return $ eq m
        ">"  -> return $ gt m
        "<"  -> return $ lt m
        ">=" -> return $ ge m
        "<=" -> return $ le m
        "!=" -> return $ ne m
        "||" -> return $ Interpret.or m
        "&&" -> return $ Interpret.and m
        "!"  -> return $ Interpret.not m
        "dup"-> do
            let checked = M.require 1 m
            if fault checked then return checked else do
                let (a, m') = M.popUnsafe checked
                return $ M.push a (M.push a m')
        "drop" -> do
            let checked = M.require 1 m
            if fault checked then return checked else do
                let (_, m') = M.popUnsafe checked
                return m'
        "swap" -> do
            let checked = M.require 2 m
            if fault checked then return checked else do
                let (a, m')  = M.popUnsafe checked
                let (b, m'') = M.popUnsafe m'
                return $ M.push b (M.push a m'')
        "over" -> do
            let checked = M.require 2 m
            if fault checked then return checked else do
                let (a, m')  = M.popUnsafe checked
                let (b, m'') = M.popUnsafe m'
                return $ M.push b (M.push a (M.push b m''))
        "rot" -> do
            let checked = M.require 3 m
            if fault checked then return checked else do
                let (a, m')   = M.popUnsafe checked
                let (b, m'')  = M.popUnsafe m'
                let (c, m''') = M.popUnsafe m''
                return $ M.push b (M.push c (M.push a m'''))
        "puts" -> do
            let checked = M.require 1 m
            if fault checked then return checked else do
                let (a, m') = M.popUnsafe checked
                return $ M.put (printAtom a) m'
        "putsln" -> do
            let checked = M.require 1 m
            if fault checked then return checked else do
                let (a, m') = M.popUnsafe checked
                return $ M.put (printAtom a ++ "\n") m'
        "gets"   -> getLine >>= \x -> return $ M.push (AString x) m
        "flush"  -> M.mFlush m
        "sleep"  -> do
            let checked = M.require 1 m
            if fault checked then return checked else do
                let (a, m') = M.popUnsafe checked
                case a of
                    AInt n -> do
                        mf <- M.mFlush m'
                        M.mSleepms n mf
                    _      -> return $ M.err ("Expected int for sleep, got " ++ show a) m
        -- Probably will never happen because
        -- parser should've caught it
        _ -> error "unreachable"
    If t f -> do
        let (b, m') = truth m
        if b == Just True then evalExprs t m' else evalExprs f m'
        where truth m = case check m of
                (Just r, m') -> case r of
                    (ABool a) -> (Just a, m')
                    a -> (Nothing, M.err ("Expected boolean, got " ++ show a) m')
                (Nothing, m') -> (Nothing, m')
    Try t o -> do
        let iom = evalExprs t m
        iom >>= \m' -> if fault m' then evalExprs o m else return m'
    Take vs body -> do
        let len = length vs
        if len > length (stack m) then
            return $ M.err ("Not enough element on the stack (have " ++ show (length (stack m)) ++ " but need " ++ show len ++ ")") m
        else do
            let elems = reverse $ take len (stack m)
            let m' = M.bindl (zip vs elems) (M.dropUnsafe len m)
            evalExprs body m'
    Peek vs body -> do
        let len = length vs
        if len > length (stack m) then
            return $ M.err ("Not enough element on the stack (have " ++ show (length (stack m)) ++ " but need " ++ show len ++ ")") m
        else do
            let elems = reverse $ take len (stack m)
            let m' = M.bindl (zip vs elems) m
            evalExprs body m'

evalExprs :: [Expr] -> Machine -> IO Machine
evalExprs [] m = return m
evalExprs (e:es) m = do
    m' <- evalExpr e m
    if fault m' then return m' else evalExprs es m'

evalStmt :: Stmt -> Machine -> IO Machine
evalStmt s m = case s of
    Entry body -> evalExprs body m
    Func name _ _ body -> return $ M.bindFunc name body m

evalProgram :: Program -> Machine -> IO Machine
evalProgram [] m = return m
evalProgram (s:ss) m = do
    m' <- evalStmt (value s) m
    if fault m' then return m' else evalProgram ss m'