module Main where

import Parser (parseAll, errorUnpack, fmtErrors)
import Interpreter (Interpreter (..), evalStmts, initInterpreter )
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import qualified Control.Monad.Trans.Except as E
import qualified Control.Monad.Trans.State  as S

main :: IO ()
main = do
    args <- getArgs
    case args of
        [path] -> do
            contents <- readFile path
            case parseAll path contents of
                Left err -> hPutStrLn stderr 
                    . concatMap ("\x1b[91mParse error:\x1b[0m " ++)
                    . fmtErrors . errorUnpack $ err
                Right stmts -> do
                    result <- S.runStateT (E.runExceptT (evalStmts stmts)) initInterpreter
                    case result of
                        (Left e, _)    -> hPutStrLn stderr ("\x1b[91mRuntime Error:\x1b[0m " ++ e)
                        (Right (), i') -> putStr $ concat $ reverse $ bufio i'
        _ -> putStrLn "0.1.0"
