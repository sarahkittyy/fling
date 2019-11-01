-- | Defines the interpreter engine for fling.
module Fling.Interpreter where
    
import Fling.Grammar

-- | The main environment, where all persistent data is stored
data Env = Env {

} deriving (Show)

newEnv :: Env
newEnv = Env {  }

-- | Reduces an expression to a literal
evalExpr :: Expression -> Literal
evalExpr (Literal l) = l 
evalExpr (Wrapped expr) = evalExpr expr