-- | Final wrapped option parser for use in Main
module Opts.Parse where
    
import Opts.Grammar
import Parser.Parser
import Data.List

-- | Converts all the input args to parseable options
fromOpts :: FlagOnly -> ValueOnly -> [String] -> Either String [Option]
fromOpts fo vo opts =
    let input = intercalate " " opts
    in case parse (parseOptions fo vo) input of
        Left err -> Left err
        Right (res, rest) ->
            if null rest
                then Left "Error in arguments given."
                else Right res