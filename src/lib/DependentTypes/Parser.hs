module DependentTypes.Parser
       ( DependentTypes.Parser.parse
       ) where

import DependentTypes.Data
import Text.ParserCombinators.Parsec

parse :: String -> Either ParseError Program
parse input = Text.ParserCombinators.Parsec.parse parseProgram "" input

parseProgram :: Parser Program
parseProgram = return $ Program []
