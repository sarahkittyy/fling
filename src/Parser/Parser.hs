{-# LANGUAGE InstanceSigs #-}

module Parser.Parser
( Parser
, runParser
, failure
, empty
, (<|>)
, item
, satisfies
, char
, some
, many
, anyOf
, noneOf
, string
, digit
, natural
, integer
, number
, nOf
, zeroOrOne
, spacing
, between
, quoted
, upTo
, upToI
, parens
)where
    
import Control.Applicative hiding (some, many)
import Data.Char
import Control.Monad (when)
    
-- | Parser data type.
data Parser a = Parser { parse :: String -> Either String (a, String) }

-- | Runs a parser, throwing an error if it occurs
runParser :: Parser a -> String -> a
runParser parser input =
    case parse parser input of
        Left err -> error $ err
        Right (match, rest) -> match
        
instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap fn p = Parser $ \input ->
        case parse p input of
            Left err -> Left err
            Right (a, rest) -> Right $ (fn a, rest)
           
instance Applicative Parser where
    pure :: a -> Parser a
    pure n = Parser $ \input -> Right (n, input)
    
    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    pf <*> p = Parser $ \input ->
        case parse pf input of
            Left err -> Left err
            Right (fn, rest) -> parse (fn <$> p) rest
          
instance Monad Parser where
    return :: a -> Parser a
    return = pure
    
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    p >>= fn = Parser $ \input ->
        case parse p input of
            Left err -> Left err
            Right (match, rest) -> parse (fn match) rest
            
-- | Parser instance that always fails
failure :: String -> Parser a
failure err = Parser $ \input -> Left err
            
instance Alternative Parser where
    empty :: Parser a
    empty = failure "Parser failed."
    
    (<|>) :: Parser a -> Parser a -> Parser a
    p1 <|> p2 = Parser $ \input ->
        case parse p1 input of
            Right res -> Right res
            _ -> parse p2 input

          
-- | Parse a single character
item :: Parser Char
item = Parser $ \input ->
        if null input
            then Left "No more characters to consume in input."
            else Right (head input, tail input)
           
-- | Parse a character satifsying a predicate
satisfies :: (Char -> Bool) -> Parser Char
satisfies pred = item >>= \c -> if pred c
                                    then return c
                                    else failure $ "Character " ++ [c] ++ " did not satisfy predicate."
                                   
-- | Matches a character
char :: Char -> Parser Char
char = satisfies . (==)

-- | Matches 1 or more of a given parser
some :: Parser a -> Parser [a]
some p = (:) <$> p <*> (some p <|> pure [])

-- | 0 or more of a given parser
many :: Parser a -> Parser [a]
many p = some p <|> pure [] 

-- | Matches any of the given characters.
anyOf :: [Char] -> Parser Char
anyOf chars = satisfies (`elem` chars)

-- | Matches none of the given characters
noneOf :: [Char] -> Parser Char
noneOf chars = satisfies $ not . flip elem chars

-- | Matches a specific string of characters
string :: String -> Parser String
string "" = pure ""
string (c:cs) = char c >>= \rc -> string cs >>= \rcs -> return (rc:rcs) 

-- | Matches a single digit
digit :: Parser Char
digit = satisfies isDigit

-- | Matches a whole positive integer
natural :: Parser String
natural = some digit

-- | Matches a positive or negative integer.
integer :: Parser String
integer = do
    sign <- zeroOrOne (char '-')
    num <- natural
    return $ sign ++ num
    
-- | Matches a positive or negative decimal number.
number :: Parser String
number = do
    sign <- zeroOrOne (char '-')
    whole <- natural <|> pure ""
    decimal <- ( do
        pt <- char '.'
        fpart <- natural
        return $ pt:fpart ) <|> pure ""
    when (null whole && null decimal) $ failure "No numbers at all given..? >w<"
    return $ sign ++ whole ++ decimal

-- | Matches a specific count of characters
nOf :: Int -> Parser a -> Parser [a]
nOf 0 _ = pure []
nOf n p = do
    c <- p
    rest <- nOf (n - 1) p <|> (failure $ "Need " ++ (show n) ++ " more matches of parser, yet stream could not satisfy.")
    return (c:rest)
    
-- | Tries to parse 0 or one of the given parser
zeroOrOne :: Parser a -> Parser [a] 
zeroOrOne p = nOf 1 p <|> pure []

-- | Parses some spacing
spacing :: Parser String
spacing = some $ satisfies (isSpace)

-- | Matches items until a given parser matches
upTo :: Parser a -> Parser String
upTo p = (p >> pure "") <|> do
                c <- item
                cs <- upTo p
                return $ c:cs
               
-- | Matches up until the given parser matches, including it in the result
upToI :: Parser String -> Parser String
upToI p = p <|> do
    c <- item
    cs <- upToI p 
    return $ c:cs
    
-- | Matches a parser between two parsers
between :: Parser l -> Parser r -> Parser v -> Parser v
between l r v = do
    l <|> (failure $ "Could not match between's start value.")
    res <- v
    r <|> (failure $ "Could not match between's end value.")
    return res

-- | Matches anything surrounded in parentheses
parens :: Parser a -> Parser a
parens = between (char '(') (char ')')

-- | Matches quoted text
quoted :: Parser String
quoted = do
    char '"'
    upToI $ (satisfies (/='\\')) >>= \cf -> char '"' >> return (cf:"")