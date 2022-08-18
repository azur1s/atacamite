module Main where

import Parse (parseProgram)
import Machine (initM, Machine(..))
import Interpret (evalProgram)
import System.IO (hFlush, stdout)
import System.Environment (getArgs)

version :: String
version = "atacamite version 0.1.0"

main :: IO ()
main = do
    args <- getArgs
    case args of
        [file] -> do
            contents <- readFile file
            case parseProgram file (contents ++ "\n") of
                Left err -> putStrLn $ "Parse error " ++ show err
                Right prog -> do
                    -- putStrLn $ fmtProgram prog
                    let m = initM
                    let m' = evalProgram prog m
                    m' >>= \x -> putStr $ concat $ reverse $ output x
        _ -> putStrLn version