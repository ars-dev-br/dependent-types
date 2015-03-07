{-|
Module:      DependentTypes.Parser
Description: Parsing functions.
-}

module DependentTypes.Parser
       ( DependentTypes.Parser.parse
       ) where

import Control.Monad
import DependentTypes.Data
import Text.Parsec
import Text.Parsec.String

-- | Parses the input and returns either a ParseError or a grammatically valid
-- Program.
parse :: String -> Either ParseError Program
parse = Text.Parsec.parse parseProgram ""

-- | Parses the program as a whole (i.e. a whole file).
parseProgram :: Parser Program
parseProgram = liftM Program $ parseToplevel `endBy` toplevelDelimiter
  where
    toplevelDelimiter = spaces >> char '.' >> spaces >> skipMany comment >> spaces
    comment = string "{-" >> manyTill anyChar (try (string "-}"))

-- | Parses toplevel constructs.
parseToplevel :: Parser Toplevel
parseToplevel = parseType <|> parseFunc <|> parsePrint

-- | Parses a type declaration.
parseType :: Parser Toplevel
parseType = do
  _ <- string "type"
  _ <- spaces
  typeId <- many letter
  _ <- spaces
  _ <- char ':'
  _ <- spaces
  signature <- string "Type"
  return $ Type (TypeId typeId) (Signature [TypeId signature])

-- | Parses a function declaration.
parseFunc :: Parser Toplevel
parseFunc = do
  _ <- string "func"
  return $ Func (Id "func") (Signature [TypeId "Nat"])

-- | Parses a print declaration.
parsePrint :: Parser Toplevel
parsePrint = do
  _ <- string "print"
  return $ Print
