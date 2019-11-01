{-# LANGUAGE TupleSections #-}

-- | Defines the interpreter engine for fling.
module Fling.Interpreter where
    
import qualified Fling.Grammar as G (Statement(Function))
import Fling.Grammar hiding (Function)
import Parser.Parser
import Data.Maybe (isJust, fromJust)

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
-- | Insert a function into the environment
insertFunction :: Env -> String -> Function -> Env
insertFunction (Env fns bindings) name fn = Env ((name,fn):fns) bindings
-- | Insert new bindings into the environment
insertBinding :: Env -> String -> Literal -> Env
insertBinding (Env fns bindings) name val = Env fns ((name, val):bindings)

-- | Parse, Interpret and run a single line of flang code. (For the repl)
runLine :: Env -> String -> Either String (String, Env)
runLine env str = tryStatement <> (tryExpr env)
    where
        tryExpr env = case parse parseExpression str of
                        -- If it's an expression
                        Right (expr, _) -> (,env) <$> rShow <$> (evalExpr env expr)
                        Left err -> Left err
    
        tryStatement = case parse parseStatement str of
                        -- If it's a statement
                        Right (statement, _) -> ("<3",) <$> evalStatement env statement
                        Left err -> Left err

        onFirst :: (a, b) -> (a -> c) -> (c, b)
        onFirst (a, b) fn = (fn a, b)

-- | Reduces an expression to a literal
evalExpr :: Env -> Expression -> Either String Literal
evalExpr env (Literal l) = Right l 
evalExpr env (Wrapped expr) = evalExpr env expr
evalExpr env (Variable v) =
    case lookup v (bindings env) of
        Nothing -> Left $ "Variable " ++ v ++ " not defined."
        Just val -> Right (val, env)

evalExpr env (Call name params) =
    case lookup name (functions env) of
        Nothing -> Left $ "Function " ++ name ++ " not defined."
        Just fn ->
            case flip evalExpr (body fn) <$> bindValues (args fn) params env of
                Nothing -> Left "Could not evalute inner params"
                Just res -> res
    where
        bindValues :: [String] -> [Expression] -> Env -> Maybe Env
        bindValues names vals env =
            let zipped = zip names (map (evalExpr' env) vals)
            in if any (not . isJust . snd) zipped
                    then Nothing
                    else Just $ foldr (flip insertBinding') env $ map (\(s, le) -> (s, fromJust le)) zipped
            where
                insertBinding' :: Env -> (String, Literal) -> Env
                insertBinding' env = uncurry (insertBinding env)

                evalExpr' :: Env -> Expression -> Maybe Literal
                evalExpr' env expr =
                    case evalExpr env expr of
                        Left err -> Nothing
                        Right res -> Just res

    
-- | Evaluates a statement
evalStatement :: Env -> Statement -> Either String Env
evalStatement env (G.Function name args body) = Right $ insertFunction env name (Function args body)