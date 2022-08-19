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
index, headtail, explode :: Machine -> Machine

add m = case check2 m of
    (Just r, m') -> case r of
        (AInt a, AInt b)     -> M.push (AInt (a + b)) m'
        (AFloat a, AFloat b) -> M.push (AFloat (a + b)) m'
        (a, b) -> binopErr "+" a b m'
    (Nothing, m') -> m'
sub m = case check2 m of
    (Just r, m') -> case r of
        (AInt a, AInt b)     -> M.push (AInt (b - a)) m'
        (AFloat a, AFloat b) -> M.push (AFloat (b - a)) m'
        (a, b) -> binopErr "-" a b m'
    (Nothing, m') -> m'

mul m = case check2 m of
    (Just r, m') -> case r of
        (AInt a, AInt b)     -> M.push (AInt (a * b)) m'
        (AFloat a, AFloat b) -> M.push (AFloat (a * b)) m'
        (a, b) -> binopErr "*" a b m'
    (Nothing, m') -> m'

div m = case check2 m of
    (Just r, m') -> case r of
        (AInt a, AInt b)     -> M.push (AInt (Prelude.div b a)) m'
        (AFloat a, AFloat b) -> M.push (AFloat (b / a)) m'
        (a, b) -> binopErr "/" a b m'
    (Nothing, m') -> m'

mod m = case check2 m of
    (Just r, m') -> case r of
        (AInt a, AInt b)     -> M.push (AInt (Prelude.mod b a)) m'
        (a, b) -> binopErr "%" a b m'
    (Nothing, m') -> m'

exp m = case check2 m of
    (Just r, m') -> case r of
        (AInt a, AInt b)     -> M.push (AInt (b ^ a)) m'
        (AFloat a, AFloat b) -> M.push (AFloat (b ** a)) m'
        (a, b) -> binopErr "^" a b m'
    (Nothing, m') -> m'

eq m = case check2 m of
    (Just r, m') -> case r of
        (AInt a, AInt b)       -> M.push (ABool (a == b)) m'
        (AFloat a, AFloat b)   -> M.push (ABool (a == b)) m'
        (ABool a, ABool b)     -> M.push (ABool (a == b)) m'
        (AString a, AString b) -> M.push (ABool (a == b)) m'
        (AList a, AList b)     -> M.push (ABool (a == b)) m'
        (AString a, AList b)   -> M.push (ABool (length a == length b)) m'
        (AList a, AString b)   -> M.push (ABool (length a == length b)) m'
        (a, b) -> binopErr "=" a b m'
    (Nothing, m') -> m'

gt m = case check2 m of
    (Just r, m') -> case r of
        (AInt a, AInt b)       -> M.push (ABool (a > b)) m'
        (AFloat a, AFloat b)   -> M.push (ABool (a > b)) m'
        (a, b) -> binopErr ">" a b m'
    (Nothing, m') -> m'

lt m = case check2 m of
    (Just r, m') -> case r of
        (AInt a, AInt b)       -> M.push (ABool (a < b)) m'
        (AFloat a, AFloat b)   -> M.push (ABool (a < b)) m'
        (a, b) -> binopErr "<" a b m'
    (Nothing, m') -> m'

ge m = case check2 m of
    (Just r, m') -> case r of
        (AInt a, AInt b)       -> M.push (ABool (a >= b)) m'
        (AFloat a, AFloat b)   -> M.push (ABool (a >= b)) m'
        (a, b) -> binopErr ">=" a b m'
    (Nothing, m') -> m'

le m = case check2 m of
    (Just r, m') -> case r of
        (AInt a, AInt b)       -> M.push (ABool (a <= b)) m'
        (AFloat a, AFloat b)   -> M.push (ABool (a <= b)) m'
        (a, b) -> binopErr "<=" a b m'
    (Nothing, m') -> m'

ne m = case check2 m of
    (Just r, m') -> case r of
        (AInt a, AInt b)       -> M.push (ABool (a /= b)) m'
        (AFloat a, AFloat b)   -> M.push (ABool (a /= b)) m'
        (a, b) -> binopErr "!=" a b m'
    (Nothing, m') -> m'

or m = case check2 m of
    (Just r, m') -> case r of
        (ABool a, ABool b) -> M.push (ABool (a || b)) m'
        (a, b) -> binopErr "or" a b m'
    (Nothing, m') -> m'

and m = case check2 m of
    (Just r, m') -> case r of
        (ABool a, ABool b) -> M.push (ABool (a && b)) m'
        (a, b) -> binopErr "and" a b m'
    (Nothing, m') -> m'

not m = case check m of
    (Just r, m') -> case r of
        (ABool a) -> M.push (ABool (Prelude.not a)) m'
        a -> unaopErr "not" a m'
    (Nothing, m') -> m'

index m = case check2 m of
    (Just r, m') -> case r of
        (AInt a, AList b) -> if length b <= a then M.err "index out of bounds while indexing list" m' else
            M.push (b !! a) m'
        (AInt a, AString b) -> if length b <= a then M.err "index out of bounds while indexing string" m' else
            M.push (AString [b !! a]) m'
        (a, b) -> binopErr "index" a b m'
    (Nothing, m') -> m'

headtail m = case check m of
    (Just r, m') -> case r of
        (AList a) -> if null a then M.err "head of empty list" m' else
            M.push (head a) (M.push (AList (tail a)) m')
        (AString a) -> if null a then M.err "head of empty string" m' else
            M.push (AString [head a]) (M.push (AString (tail a)) m')
        a -> unaopErr "head" a m'
    (Nothing, m') -> m'

explode m = case check m of
    (Just r, m') -> case r of
        (AString a) -> M.push (AList (map (\x -> AString [x]) a)) m'
        a -> unaopErr "explode" a m'
    (Nothing, m') -> m'

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
        "?"  -> return $ M.push (AString (show $ stack m)) m
        "@"  -> return $ index m
        "**" -> return $ headtail m
        "*!" -> return $ explode m
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
        e -> error ("unreachable: " ++ e)
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
    Import _ _ -> return m

evalProgram :: Program -> Machine -> IO Machine
evalProgram [] m = return m
evalProgram (s:ss) m = do
    m' <- evalStmt (value s) m
    if fault m' then return m' else evalProgram ss m'