{-# LANGUAGE ScopedTypeVariables, InstanceSigs #-}

-- | Defines the grammar for the fling language.
module Fling.Grammar where
    
import Parser.Parser
import Data.Char

-- Used to display values to the user's side in a repl.
class ReplShow a where
    rShow :: a -> String

-- | Statements are anything that doesn't reduce.
data Statement = Function String [String] Expression
                    deriving (Show)
                    
parseStatement :: Parser Statement
parseStatement = parseFunction
                    <|> (failure "Invalid statement!")
    where
        parseFunction :: Parser Statement
        parseFunction = do
            string "def"
            spacing
            name <- validVarName
            args <- parens $ sepBy spacing (validVarName)
            char '{'
            body <- parseExpression
            char '}'
            
            return $ Function name args body
    
-- | Expressions are anything that reduces to a literal
data Expression = Literal Literal
                    | Variable String
                    | Wrapped Expression
                    deriving (Show)

parseExpression :: Parser Expression
parseExpression = (Literal <$> parseLiteral)
                    <|> parseVariable
                    <|> parseWrapped
                    <|> (failure "Invalid expression!")
    where
        parseWrapped :: Parser Expression
        parseWrapped = Wrapped <$> parens parseExpression

        parseVariable :: Parser Expression
        parseVariable = Variable <$> validVarName

-- | Matches a valid variable identifier.
validVarName :: Parser String
validVarName = (:) <$> satisfies isAlpha <*> (many $ satisfies isAlphaNum)

-- | Literals are the most basic form of expression
data Literal = Number String 
                | String String
                deriving (Show)

parseLiteral :: Parser Literal
parseLiteral = parseNumber
                <|> parseString
                <|> (failure "Invalid literal!")
    where
        parseNumber :: Parser Literal
        parseNumber = Number <$> number

        parseString :: Parser Literal
        parseString = String <$> quoted


instance ReplShow Literal where
    rShow :: Literal -> String
    rShow (Number n) = n
    rShow (String s) = "\"" ++ s ++ "\""