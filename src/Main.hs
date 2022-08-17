module Main where

import Parse (parseProgram)
import Types (fmtProgram)
import System.Environment (getArgs)

version :: String
version = "atacamite version 0.1.0"

main :: IO ()
main = do
    args <- getArgs
    case args of
        [file] -> do
            contents <- readFile file
            case parseProgram file contents of
                Left err -> putStrLn $ "Parse error " ++ show err
                Right prog -> putStrLn $ fmtProgram prog
        _ -> putStrLn version