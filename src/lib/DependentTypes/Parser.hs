module DependentTypes.Parser
       ( DependentTypes.Parser.parse
       ) where

import Control.Monad
import DependentTypes.Data
import Text.Parsec
import Text.Parsec.String

parse :: String -> Either ParseError Program
parse input = Text.Parsec.parse parseProgram "" input

parseProgram :: Parser Program
parseProgram = liftM Program $ parseToplevel `endBy` toplevelDelimiter

parseToplevel :: Parser TopLevel
parseToplevel = parseType

parseType :: Parser TopLevel
parseType = do
  _ <- string "type"
  _ <- spaces
  typeId <- many letter
  _ <- spaces
  _ <- char ':'
  _ <- spaces
  signature <- string "Type"
  return $ Type (TypeId typeId) [TypeId signature]

toplevelDelimiter = do
  spaces
  char '.'
  spaces
