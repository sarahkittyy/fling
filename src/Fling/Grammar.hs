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
                    | Binding String Expression
                    deriving (Show)
                    
parseStatement :: Parser Statement
parseStatement = parseFunction
                    <|> parseBinding
                    <|> (failure "Invalid statement!")
    where
        parseBinding :: Parser Statement
        parseBinding = do
            string "def"
            spacing
            name <- validVarName
            spacing
            expr <- parseExpression
            return $ Binding name expr
        
        parseFunction :: Parser Statement
        parseFunction = do
            string "def"
            spacing
            name <- validVarName
            zeroOrOne spacing
            args <- parens $ (zeroOrOne spacing) >> (sepBy spacing (validVarName) <* (zeroOrOne spacing))
            zeroOrOne spacing
            char '{'
            zeroOrOne spacing
            body <- parseExpression
            zeroOrOne spacing
            char '}'
            
            return $ Function name args body
    
-- | Expressions are anything that reduces to a literal
data Expression = Literal Literal
                    | Call String [Expression]
                    | Variable String
                    | Wrapped Expression
                    deriving (Show)

parseExpression :: Parser Expression
parseExpression = (Literal <$> parseLiteral)
                    <|> parseCall
                    <|> parseVariable
                    <|> parseWrapped
                    <|> (failure "Invalid expression!")
    where
        parseWrapped :: Parser Expression
        parseWrapped = Wrapped <$> parens parseExpression
        
        parseCall :: Parser Expression
        parseCall = do
            name <- validVarName
            zeroOrOne spacing
            args <- parens $ (zeroOrOne spacing) >> (sepBy spacing parseExpression <* (zeroOrOne spacing))
            return $ Call name args

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