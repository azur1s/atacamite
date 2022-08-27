module Main where

import Data.Either (isLeft, isRight)
import Data.List (isPrefixOf)
import Data.Text (Text)
import Data.Void (Void)
import Interpreter (Interpreter (..), evalStmts, initInterpreter )
import Parser (parseAll, errorUnpack, fmtParseError)
import System.Environment (getArgs)
import System.Directory (canonicalizePath, setCurrentDirectory, getHomeDirectory)
import System.FilePath (takeDirectory, (</>))
import System.IO (hPutStrLn, stderr)
import Text.Megaparsec (ParseErrorBundle)
import qualified Control.Monad.Trans.Except as E
import qualified Control.Monad.Trans.State  as S
import qualified Types                      as T

parseFile :: FilePath -> IO (Either (ParseErrorBundle Text Void) [T.Statement])
parseFile path = do
    -- Set working directory to the file's directory
    fullPath <- canonicalizePath path
    setCurrentDirectory $ takeDirectory fullPath

    contents <- readFile fullPath
    case parseAll path (contents ++ "\n") of
        Left err -> return $ Left err
        Right p -> do
            let imports = getImports p

            home <- getHomeDirectory
            -- Map all import that starts with "std/.." to their full path "~/.atacamite/.."
            let coreFull = map
                    (\c -> home </> ".atacamite" </> drop 4 c)  -- drop 4 is to remove "std/"
                    (filter (isPrefixOf "std/") imports)
            let paths' = coreFull ++ filter (not . isPrefixOf "std/") imports

            -- Parse all files
            progs' <- mapM canonicalizePath paths' >>= \x -> mapM parseFile x
            let errs = filter isLeft progs'
            if null errs then do
                let ps = map (\(Right p) -> p) (filter isRight progs')
                let all = concat ps ++ p
                return $ Right all
                else return $ Left $ head (map (\(Left e) -> e) errs)
    where
        getImports = map
            (\(T.Use path) -> path ++ ".ata") . filter (\s -> case s of T.Use _ -> True; _ -> False)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [path] -> do
            prog <- parseFile path
            case prog of
                Left err -> hPutStrLn stderr
                    . concatMap ("\x1b[91mParse error:\x1b[0m " ++)
                    . fmtParseError . errorUnpack $ err
                Right stmts -> do
                    result <- S.runStateT (E.runExceptT (evalStmts stmts)) initInterpreter
                    case result of
                        (Left e, _)    -> hPutStrLn stderr ("\x1b[91mRuntime Error:\x1b[0m " ++ e)
                        (Right (), i') -> putStr $ concat $ reverse $ bufio i'
        _ -> putStrLn "0.1.0.0"
