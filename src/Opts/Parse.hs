-- | Final wrapped option parser for use in Main
module Opts.Parse where
    
import Opts.Grammar
import Parser.Parser
import Data.List

-- | Converts all the input args to parseable options
fromOpts :: Program -> [String] -> Either String [Option]
fromOpts prog opts =
    let input = intercalate " " opts
    in case parse (parseOptions prog) input of
        Left err -> Left err
        Right (res, rest) ->
            if not $ null rest
                then Left "Error in arguments given."
                else Right res