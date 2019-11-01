{-# LANGUAGE ScopedTypeVariables #-}

-- | Defines the grammar for the fling language.
module Fling.Grammar where
    
import Parser.Parser

-- | Expressions are anything that reduces to a literal
data Expression = Literal Literal | Wrapped Expression deriving (Show)
parseExpression :: Parser Expression
parseExpression = (Literal <$> parseLiteral)
                    <|> parseWrapped
    where
        parseWrapped :: Parser Expression
        parseWrapped = Wrapped <$> parens parseExpression

-- | Literals are the most basic form of expression
data Literal = Number String | String String deriving (Show)
parseLiteral :: Parser Literal
parseLiteral = parseNumber
                <|> parseString
    where
        parseNumber :: Parser Literal
        parseNumber = Number <$> number
        parseString :: Parser Literal
        parseString = String <$> quoted