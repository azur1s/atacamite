module Machine where

import Data.Map (Map)
import qualified Data.Map as Map
import System.IO (hFlush, stdout)
import Types (Value, Expr)

data Machine = Machine
    { fault  :: Bool
    , stack  :: [Value]
    , binds  :: Map String Value
    , funcs  :: Map String [Expr]
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

drop :: Int -> Machine -> Machine
drop n m = m { stack = Prelude.drop n (stack m) }

push :: Value -> Machine -> Machine
push v m = m { stack = v : stack m }

pop :: Machine -> (Maybe Value, Machine)
pop m = case stack m of
    [] -> (Nothing, m { fault = True })
    (v : vs) -> (Just v, m { stack = vs })

pop2 :: Machine -> (Maybe Value, Maybe Value, Machine)
pop2 m = case stack m of
    [] -> (Nothing, Nothing, m { fault = True })
    (v : vs) -> case vs of
        [] -> (Just v, Nothing, m { stack = vs })
        (v' : vs') -> (Just v, Just v', m { stack = vs' })

pop3 :: Machine -> (Maybe Value, Maybe Value, Maybe Value, Machine)
pop3 m = case stack m of
    [] -> (Nothing, Nothing, Nothing, m { fault = True })
    (v : vs) -> case vs of
        [] -> (Just v, Nothing, Nothing, m { stack = vs })
        (v' : vs') -> case vs' of
            [] -> (Just v, Just v', Nothing, m { stack = vs' })
            (v'' : vs'') -> (Just v, Just v', Just v'', m { stack = vs'' })

bind :: String -> Value -> Machine -> Machine
bind s v m = m { binds = Map.insert s v (binds m) }

bindl :: [(String, Value)] -> Machine -> Machine
bindl bs m = foldl (\m (s, v) -> bind s v m) m bs

get :: String -> Machine -> Maybe Value
get s m = Map.lookup s (binds m)

bindFunc :: String -> [Expr] -> Machine -> Machine
bindFunc s body m = m { funcs = Map.insert s body (funcs m) }

getFunc :: String -> Machine -> Maybe [Expr]
getFunc s m = Map.lookup s (funcs m)

-- | Helper functions

putBuf :: Machine -> IO Machine
putBuf m = do
    putStr $ concat $ reverse $ output m
    hFlush stdout
    return m { output = [] }

-- Somehow apply-family function (apply, apply2, ..) doesn't actually pop the stack
-- i.e: swap -> apply2 (\x y -> Left $ m { stack = y : x : stack m }) m
-- would result in stack = [y, x, x, y]
-- However the workaround is to just call pop on it
--      swap -> apply2 (\x y -> Left $ m { stack = y : x : stack (drop $ drop m) }) m
-- I don't know if this is intended behaviour or not, if it is, please remove this comment
apply :: (Value -> Either Machine Value) -> Machine -> Machine
apply f m0 = case pop m0 of
    (Just v, m1) -> case f v of
        Left m2 -> m2
        Right v' -> push v' m1
    (Nothing, m1) -> err "Stack underflow" m1

apply2 :: (Value -> Value -> Either Machine Value) -> Machine -> Machine
apply2 f m0 = case pop m0 of
    (Just v1, m1) -> case pop m1 of
        (Just v2, m2) -> case f v1 v2 of
            Left m3 -> m3
            Right v' -> push v' m2
        (Nothing, m2) -> err "Stack underflow" m2
    (Nothing, m1) -> err "Stack underflow" m1

apply3 :: (Value -> Value -> Value -> Either Machine Value) -> Machine -> Machine
apply3 f m0 = case pop m0 of
    (Just v1, m1) -> case pop m1 of
        (Just v2, m2) -> case pop m2 of
            (Just v3, m3) -> case f v1 v2 v3 of
                Left m4 -> m4
                Right v' -> push v' m3
            (Nothing, m3) -> err "Stack underflow" m3
        (Nothing, m2) -> err "Stack underflow" m2
    (Nothing, m1) -> err "Stack underflow" m1