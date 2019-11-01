-- | Defines the grammar for parsing command-line options
module Opts.Grammar where
    
import Parser.Parser
import Data.Char
import Control.Monad (when)

-- | Args that can *ONLY* be given as flags with no args.
type FlagOnly = [String]
isFlagOnly :: String -> FlagOnly -> Bool
isFlagOnly = elem
-- | Args that can *ONLY* be given with corresponding values.
type ValueOnly = [String]
isValueOnly :: String -> ValueOnly -> Bool
isValueOnly = elem

data Argument = Short Char | Long String deriving (Show)
argName :: Argument -> String
argName (Short ch) = [ch]
argName (Long str) = str

-- | The main program data! Includes positional arg count, and flags that are only value / only flag.
data Program = Program { flagOnly :: FlagOnly
                       , valueOnly :: ValueOnly }

data Option = Flag Argument | Value Argument String | Positional String deriving (Show)

-- | Parse all cli opts.
parseOptions :: Program -> Parser [Option]
parseOptions (Program fonly vonly) = many $ (parseValue fonly <|> parseFlag vonly <|> parsePositional) >>= \opt -> (zeroOrOne spacing) >> return opt

-- | Parse a single argument structure, either -o or --option
parseArgument :: Parser Argument
parseArgument = parseLong <|> parseShort
    where
        parseLong :: Parser Argument
        parseLong = do
            string "--"
            name <- some (satisfies isAlpha)
            return $ Long name
        
        parseShort :: Parser Argument
        parseShort = do
            char '-'
            name <- item
            return $ Short name

-- | Parses a key-value pair, with argument key and value.
parseValue :: FlagOnly -> Parser Option
parseValue fo = do
    arg <- parseArgument
    when (isFlagOnly (argName arg) fo) $ failure "This option is specified as flag-only"
    string "=" <|> spacing
    val <- some (satisfies isAlphaNum)
    return $ Value arg val
            
-- | Parses a flag, with no argument 
parseFlag :: ValueOnly -> Parser Option
parseFlag vo = Flag <$> do
    arg <- parseArgument
    when (isValueOnly (argName arg) vo) $ failure "This option is specified as value-only." 
    return arg

-- | Parses positional arguments
parsePositional :: Parser Option
parsePositional = Positional <$> some (satisfies isAlphaNum)