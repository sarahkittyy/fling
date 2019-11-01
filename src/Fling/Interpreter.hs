{-# LANGUAGE TupleSections #-}

-- | Defines the interpreter engine for fling.
module Fling.Interpreter where
    
import qualified Fling.Grammar as G (Statement(Function))
import Fling.Grammar hiding (Function)
import Parser.Parser

-- | The main environment, where all persistent data is stored
data Env = Env { functions :: [(String, Function)]
               , bindings :: [(String, Literal)]
               } deriving (Show)

-- | Function to store in the environment
data Function = Function { args :: [String]
                         , body :: Expression } deriving (Show)

-- | Create an empty environment
newEnv :: Env
newEnv = Env { functions = []
             , bindings = [] }

-- | Parse, Interpret and run a single line of flang code. (For the repl)
runLine :: Env -> String -> Either String (String, Env)
runLine env str = tryStatement <> tryExpr
    where
        tryExpr = case parse parseExpression str of
                     -- If it's an expression
                    Right (expr, _) -> (flip onFirst rShow) <$> (evalExpr env expr)
                    Left err -> Left err
    
        tryStatement = case parse parseStatement str of
                        -- If it's a statement
                        Right (statement, _) -> ("<3",) <$> evalStatement env statement
                        Left err -> Left err

        onFirst :: (a, b) -> (a -> c) -> (c, b)
        onFirst (a, b) fn = (fn a, b)

-- | Reduces an expression to a literal
evalExpr :: Env -> Expression -> Either String (Literal, Env)
evalExpr env (Literal l) = Right $ (l, env) 
evalExpr env (Wrapped expr) = evalExpr env expr
evalExpr env (Variable v) =
    case lookup v (bindings env) of
        Nothing -> Left $ "Variable " ++ v ++ " not defined."
        Just val -> Right (val, env)
    
-- | Evaluates a statement
evalStatement :: Env -> Statement -> Either String Env
evalStatement env (G.Function name args body) = Right $ Env { functions=(functions env) ++ [(name, Function args body)]
                                                          , bindings= (bindings env) }