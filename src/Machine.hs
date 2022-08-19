module Machine where

import Control.Concurrent (threadDelay)
import Data.Map (Map)
import Parse (Atom(..), Body)
import System.IO (hFlush, stdout)
import qualified Data.Map as Map

data Machine = Machine
    { fault  :: Bool
    , stack  :: [Atom]
    , funcs  :: Map String Body
    , binds  :: Map String Atom
    , output :: [String]
    , errors :: [String]
    } deriving (Show)

initM :: Machine
initM = Machine
    { fault  = False
    , stack  = []
    , funcs  = Map.empty
    , binds  = Map.empty
    , output = []
    , errors = []
    }

-- | IO operations

put :: String -> Machine -> Machine
put s m = m { output = s : output m }

err :: String -> Machine -> Machine
err s m = m { errors = ("Error: " ++ s) : errors m
            , fault = True }

mFlush :: Machine -> IO Machine
mFlush m = do
    putStr $ concat $ reverse $ output m
    hFlush stdout
    return m { output = [] }

mSleepms :: Int -> Machine -> IO Machine
mSleepms n m = do
    threadDelay (n * 1000) -- microseconds -> milliseconds
    return m

cmpStack :: Machine -> Machine -> Int -> Int
cmpStack m1 m2 argc = do
    let r = length (stack m1) - length (stack m2)
    if r < argc then argc else r

-- | Stack operations

require :: Int -> Machine -> Machine
require n m = if size m < n
    then err ("Stack underflow: " ++ show n ++ " required, " ++ show (size m) ++ " available") m
    else m

push :: Atom -> Machine -> Machine
push a m = m { stack = a : stack m }

popUnsafe :: Machine -> (Atom, Machine)
popUnsafe m = (head $ stack m, m { stack = tail $ stack m })

dropUnsafe :: Int -> Machine -> Machine
dropUnsafe len m = m { stack = drop len $ stack m }

pop :: Machine -> (Maybe Atom, Machine)
pop m = do
    let m' = require 1 m
    if fault m then (Nothing, m') else do
        let (a, m'') = popUnsafe m'
        (Just a, m'')

size :: Machine -> Int
size = length . stack

-- | Functions & Bindings

bindFunc :: String -> Body -> Machine -> Machine
bindFunc s body m = m { funcs = Map.insert s body (funcs m) }

getFunc :: String -> Machine -> Maybe Body
getFunc s m = Map.lookup s (funcs m)

bind :: String -> Atom -> Machine -> Machine
bind s v m = m { binds = Map.insert s v (binds m) }

unbind :: String -> Machine -> Machine
unbind s m = m { binds = Map.delete s (binds m) }

bindl :: [(String, Atom)] -> Machine -> Machine
bindl bs m = foldl (\m (s, v) -> bind s v m) m bs

unbindl :: [String] -> Machine -> Machine
unbindl bs m = foldl (flip unbind) m bs

get :: String -> Machine -> Maybe Atom
get s m = Map.lookup s (binds m)
