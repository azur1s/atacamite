module Machine where

import Data.Map (Map)
import qualified Data.Map as Map
import Types (Value, Expr)

data Machine = Machine
    { fault  :: Bool
    , stack  :: [Value]
    , binds  :: Map String Value
    , funcs  :: Map String Expr
    , output :: [String]
    } deriving (Show)

initMachine :: Machine
initMachine = Machine
    { fault  = False
    , stack  = []
    , binds  = Map.empty
    , funcs  = Map.empty
    , output = []
    }

out :: String -> Machine -> Machine
out s m = m { output = s : output m }

err :: String -> Machine -> Machine
err s m = m { output = ("Error: " ++ s) : output m
            , fault = True }

push :: Value -> Machine -> Machine
push v m = m { stack = v : stack m }

pop :: Machine -> (Maybe Value, Machine)
pop m = case stack m of
    [] -> (Nothing, m { fault = True })
    (v : vs) -> (Just v, m { stack = vs })

bind :: String -> Value -> Machine -> Machine
bind s v m = m { binds = Map.insert s v (binds m) }

-- | Helper functions

finalize :: Machine -> String
finalize m = concat $ reverse $ output m

apply :: (Value -> Either Machine Value) -> Machine -> Machine
apply f m = case pop m of
    (Just x, m') -> case f x of
        Left m'' -> m''
        Right z   -> push z m'
    _ -> err "Stack underflow" m

apply2 :: (Value -> Value -> Either Machine Value) -> Machine -> Machine
apply2 f m = case pop m of
    (Just x, m') -> case pop m' of
        (Just y, m'') -> case f x y of
            Left m''' -> m'''
            Right z    -> push z m''
        _ -> err "Stack underflow" m'
    _ -> err "Stack underflow" m