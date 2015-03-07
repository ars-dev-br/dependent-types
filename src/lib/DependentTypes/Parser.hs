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
parseProgram = do
  skipMany comment
  liftM Program $ parseToplevel `endBy` toplevelDelimiter
    where
      toplevelDelimiter = spaces >> char '.' >> spaces >> skipMany comment >> spaces

-- | Parses toplevel constructs.
parseToplevel :: Parser Toplevel
parseToplevel = parseType <|> parseFunc <|> parsePrint

-- | Parses a type declaration.
parseType :: Parser Toplevel
parseType = do
  string "type"
  spaces
  typeId <- many letter
  spaces
  char ':'
  spaces
  signature <- parseSignature
  constructors <- parseConstructors
  return $ Type (TypeId typeId) signature constructors

-- | Parses a function declaration.
parseFunc :: Parser Toplevel
parseFunc = do
  string "func"
  spaces
  funcId <- many letter
  spaces
  char ':'
  spaces
  signature <- many letter
  spaces
  string "where"
  return $ Func (Id funcId) (Signature [TypeId signature])

-- | Parses a print declaration.
parsePrint :: Parser Toplevel
parsePrint = do
  string "print"
  return $ Print

parseSignature :: Parser Signature
parseSignature = do
  types <- many letter `sepBy1` signatureDelimiter
  return $ Signature (map TypeId types)
    where
      signatureDelimiter = try (spaces >> string "->" >> spaces)

parseConstructors :: Parser [Constructor]
parseConstructors = option [] parseConstructor'
  where
    constructorDelimiter = spaces >> char ';' >> spaces
    parseConstructor' = do
      spaces
      string "where"
      spaces
      parseConstructor `sepBy` constructorDelimiter

parseConstructor :: Parser Constructor
parseConstructor = do
  constructorId <- many letter
  spaces
  char ':'
  spaces
  signature <- parseSignature
  return $ Constructor (Id constructorId) signature

-- | Parses a comment.
comment :: Parser ()
comment = string "{-" >> manyTill anyChar (try (string "-}")) >> spaces
