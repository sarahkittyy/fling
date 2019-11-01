-- | Defines the grammar for parsing command-line options
module Opts.Grammar where
    
import Parser.Parser
import Control.Applicative hiding (some, many)
import Data.Char
import Control.Monad (when)

-- | Args that can *ONLY* be given as flags with no args.
type FlagOnly = [String]
isFlagOnly :: String -> FlagOnly -> Bool
isFlagOnly = elem

data Argument = Short Char | Long String deriving (Show)
argName :: Argument -> String
argName (Short ch) = [ch]
argName (Long str) = str

data Option = Flag Argument | Value Argument String | Positional String deriving (Show)

-- | Parse all cli opts.
parseOptions :: FlagOnly -> Parser [Option]
parseOptions flagonly = many $ (parseValue flagonly <|> parseFlag <|> parsePositional) >>= \opt -> (zeroOrOne spacing) >> return opt

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
parseFlag :: Parser Option
parseFlag = Flag <$> parseArgument

-- | Parses positional arguments
parsePositional :: Parser Option
parsePositional = Positional <$> some (satisfies isAlphaNum)