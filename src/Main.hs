module Main where

import Data.Either (isLeft, isRight)
import Data.Maybe (isNothing, fromJust, fromMaybe)
import Data.Text (Text)
import Data.Void (Void)
import Interpret (evalProgram)
import Machine (initM, Machine(..))
import Parse (parseProgram, Program, Stmt(Import, Func), Locatable (..))
import System.Directory (canonicalizePath, getCurrentDirectory, setCurrentDirectory, getHomeDirectory)
import System.Environment (getArgs)
import System.FilePath (takeDirectory, (</>))
import System.IO (hFlush, stdout)
import Text.Megaparsec.Error (ParseErrorBundle)

getImports :: Program -> [(String, Maybe String)]
getImports p =
    map ((\(Import path rename) -> (path ++ ".ata", rename)) . value) (filter (\s -> case value s of Import _ _ -> True; _ -> False) p)

inject :: Program -> Maybe String -> Program
inject p name = map (\s -> do
    let loc = location s
    case value s of Func n a r b -> Locatable loc (Func (f name ++ n) a r b); _ -> s) p
    where f name = case name of Just n -> n ++ "::" ; Nothing -> ""

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
            let (paths, bindName) = unzip imports

            let coreList = map tail (filter (\p -> head p `elem` "#") paths)
            home <- getHomeDirectory
            let coreFull = map (\c -> home </> ".atacamite" </> c) coreList
            let paths' = coreFull ++ filter (\p -> head p `notElem` "#") paths

            fullPaths <- mapM canonicalizePath paths'
            progs' <- mapM parseFile fullPaths
            let errs = filter isLeft progs'
            if null errs then do
                let ps = map (\(Right p) -> p) (filter isRight progs')
                let all = concat (zipWith inject ps bindName) ++ p
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
                    m' >>= \x -> putStr $ concat $ reverse $ output x
        _ -> putStrLn version