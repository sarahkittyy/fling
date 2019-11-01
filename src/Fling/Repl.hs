-- | Defines the IO monad for the read-eval-print-loop
module Fling.Repl where
    
import Fling.Grammar
import Fling.Interpreter

import System.IO

-- | The repl introduction
intro :: String
intro = "Welcome to flang! <3"

-- | The repl prompt
prompt :: String
prompt = "flang ~λ "

repl :: Env -> IO ()
repl env = do
    -- prompt
    putStr prompt
    hFlush stdout
    -- read
    line <- getLine
    -- eval
    let res = runLine env line
    -- print
    case res of
        Left err -> do
            putStrLn $ "Error: " ++ err
            repl env
        Right (output, nextEnv) -> do
            putStrLn output
            repl nextEnv