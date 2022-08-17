module Interpret where

import Machine (Machine)
import qualified Machine as M
import Types

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

puts :: Machine -> Machine
puts m = M.apply (\x -> Left $ M.out (display x) m) m

putsln :: Machine -> Machine
putsln m = M.apply (\x -> Left $ M.out (display x ++ "\n") m) m

evalExpr :: Expr -> Machine -> Machine
evalExpr e m = case e of
    Push lv -> M.push (val lv) m
    Call s -> undefined
    Intr s -> case s of
        "+" -> add m
        "-" -> sub m
        "puts" -> puts m
        "putsln" -> putsln m
        _ -> M.err ("Unknown intrinsic: " ++ s) m
    If t f -> undefined
    Try t o -> undefined

evalExprs :: [Expr] -> Machine -> Machine
evalExprs es m = foldl (flip evalExpr) m es

evalStmt :: Stmt -> Machine -> Machine
evalStmt s m = case s of
    Entry body -> evalExprs (map val body) m
    Func name _ _ body -> undefined

evalProgram :: Program -> Machine -> Machine
evalProgram p m = foldl (flip evalStmt) m (map val p)
