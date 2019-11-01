module Main where
    
import Fling.Interpreter
import Fling.Repl
import Opts.Parse
import Opts.Grammar

import Control.Monad (when)
import System.Environment

flagArgs :: [String]
flagArgs = [ "i", "interactive"
           , "h", "help" ]
           
valArgs :: [String]
valArgs = [  ]

program :: Program
program = Program { flagOnly=flagArgs
                  , valueOnly=valArgs }

main :: IO ()
main = do
    opts <- fromOpts program <$> getArgs
    case opts of
        Left err -> putStrLn err >> return () 
        Right opts -> main_v opts
    where
        main_v :: [Option] -> IO ()
        main_v opts = do
            when (hasAnyOpt ["i", "interactive"] opts) (putStrLn intro >> (repl newEnv))
                
            return ()       


