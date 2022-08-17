module Machine where

import Types (Value)

data Machine = Machine
    { fault  :: Bool
    , stack  :: [Value]
    , output :: [String]
    } deriving (Show)

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