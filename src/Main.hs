module Main where

import Interpreter (Interpreter (..), evalStmts, initInterpreter )
import Parser (parseFile, errorUnpack, fmtErrors)
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import qualified Control.Monad.Trans.Except as E
import qualified Control.Monad.Trans.State  as S

main :: IO ()
main = do
    args <- getArgs
    case args of
        [path] -> do
            pres <- parseFile path
            case pres of
                Left err -> hPutStrLn stderr 
                    . concatMap ("\x1b[91mParse error:\x1b[0m " ++)
                    . fmtErrors . errorUnpack $ err
                Right stmts -> do
                    result <- S.runStateT (E.runExceptT (evalStmts stmts)) initInterpreter
                    case result of
                        (Left e, _)    -> hPutStrLn stderr ("\x1b[91mRuntime Error:\x1b[0m " ++ e)
                        (Right (), i') -> putStr $ concat $ reverse $ bufio i'
        _ -> putStrLn "0.1.0.0"
