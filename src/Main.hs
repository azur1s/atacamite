module Main where

import Interpreter ( Interpreter (..), evalStmts, initInterpreter )
import System.IO ( hPutStrLn, stderr )
import Types
import qualified Control.Monad.Trans.Except as E
import qualified Control.Monad.Trans.State  as S

main :: IO ()
main = do
    let e =
            [
                Function "print" [TypeGeneric "a"] [] [
                    Call "puts",
                    Push (unstr "\n"),
                    Call "puts"
                ],
                Function "main" [] [] [
                    Push (ValueList [ValueInt 1, ValueInt 2]),
                    Push (ValueList [ValueInt 3]),
                    Call "<",
                    Call "print"
                ]
            ]
    result <- S.runStateT (E.runExceptT (evalStmts e)) initInterpreter
    case result of
        (Left e, _)    -> hPutStrLn stderr ("\x1b[91mError:\x1b[0m " ++ e)
        (Right (), i') -> putStr $ concat $ reverse $ bufio i'