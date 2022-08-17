module Main where

import Parse (parseProgram)
import Types (fmtProgram)
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [file] -> do
            contents <- readFile file
            case parseProgram file contents of
                Left err -> putStrLn $ "Parse error " ++ show err
                Right prog -> putStrLn $ fmtProgram prog
        _ -> putStrLn "no file provided"