module Main where

import Data.Either (isLeft, isRight)
import Data.Text (Text)
import Data.Void (Void)
import Interpret (evalProgram)
import Machine (initM, Machine(..))
import Parse (parseProgram, Program, Stmt(Import), Locatable (..))
import System.Directory (canonicalizePath, setCurrentDirectory, getHomeDirectory)
import System.Environment (getArgs)
import System.FilePath (takeDirectory, (</>))
import System.IO (hPutStrLn, stderr)
import Text.Megaparsec.Error (ParseErrorBundle)

getImports :: Program -> [String]
getImports p =
    map ((\(Import path) -> path ++ ".ata") . value) (filter (\s -> case value s of Import _ -> True; _ -> False) p)

-- TODO: check circular imports
parseFile :: FilePath -> IO (Either (ParseErrorBundle Text Void) Program)
parseFile path = do
    fullPath <- canonicalizePath path
    setCurrentDirectory $ takeDirectory fullPath

    contents <- readFile fullPath
    case parseProgram path (contents ++ "\n") of
        Left err -> return $ Left err
        Right p -> do
            let imports = getImports p
            let paths = imports

            let coreList = map tail (filter (\p -> head p `elem` "#") paths)
            home <- getHomeDirectory
            let coreFull = map (\c -> home </> ".atacamite" </> c) coreList
            let paths' = coreFull ++ filter (\p -> head p `notElem` "#") paths

            fullPaths <- mapM canonicalizePath paths'
            progs' <- mapM parseFile fullPaths
            let errs = filter isLeft progs'
            if null errs then do
                let ps = map (\(Right p) -> p) (filter isRight progs')
                let all = concat ps ++ p
                return $ Right all
                else return $ Left $ head (map (\(Left e) -> e) errs)


version :: String
version = "atacamite version 0.1.0"

main :: IO ()
main = do
    args <- getArgs
    case args of
        [path] -> do
            x <- parseFile path
            case x of
                Left err -> putStrLn $ "Error: " ++ show err
                Right p -> do
                    let m = initM
                    let m' = evalProgram p m
                    m' >>= \x -> if fault x then
                        hPutStrLn stderr . concat . reverse $ errors x
                        else putStr . concat . reverse $ output x
        _ -> putStrLn version