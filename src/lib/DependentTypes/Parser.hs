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
  return $ Type typeId signature constructors

-- | Parses a function declaration.
parseFunc :: Parser Toplevel
parseFunc = do
  string "func"
  spaces
  funcId <- many letter
  spaces
  char ':'
  spaces
  signature <- parseSignature
  spaces
  string "where"
  lambdas <- parseLambdas funcId
  return $ Func funcId signature lambdas

-- | Parses a print declaration.
parsePrint :: Parser Toplevel
parsePrint = do
  string "print"
  spaces
  exp <- parsePrintExpressions
  return $ Print exp

parsePrintExpressions :: Parser Expression
parsePrintExpressions = do
  liftM ExpList $ parseArg `sepBy` printDelimiter
    where
      printDelimiter = (spaces >> char ';' >> spaces)

-- | Parses a type signature.
parseSignature :: Parser Signature
parseSignature = do
  types <- parseTypeDef `sepBy1` signatureDelimiter
  return $ Signature types
    where
      signatureDelimiter = try (spaces >> string "->" >> spaces)

-- | Parses a type
parseTypeDef :: Parser TypeDef
parseTypeDef = liftM TypeId $ many1 letter

-- | Parses a dependent type
parseDepType :: Parser TypeDef
parseDepType = do
  typeId <- many1 letter
  (Args args) <- parseArgs
  return $ DepType typeId args

-- | Parses a list of constructors for a type.
parseConstructors :: Parser [Constructor]
parseConstructors = option [] parseConstructor'
  where
    constructorDelimiter = spaces >> char ';' >> spaces
    parseConstructor' = do
      spaces
      string "where"
      spaces
      parseConstructor `sepBy` constructorDelimiter

-- | Parses a constructor for a type.
parseConstructor :: Parser Constructor
parseConstructor = do
  constructorId <- many letter
  spaces
  char ':'
  spaces
  signature <- parseSignature
  return $ Constructor constructorId (Args []) signature NoConstraint

-- | Parses all declaration bodies of a function.
parseLambdas :: String -> Parser [Lambda]
parseLambdas func = parseLambda func `sepBy1` lambdaDelimiter
  where
    lambdaDelimiter = try (spaces >> char ';' >> spaces)

-- | Parses each declaration body of a function
parseLambda :: String -> Parser Lambda
parseLambda func = do
  spaces
  _ <- string func
  args <- parseArgs
  char '='
  spaces
  exp <- parseExpression
  return $ Lambda args exp

-- | Parses all the arguments for a function declaration.
parseArgs :: Parser Args
parseArgs = do
  spaces
  liftM Args $ parseArg `endBy` argDelimiter
    where
      argDelimiter = spaces

-- | Parses each argument for a function declaration.
parseArg :: Parser Expression
parseArg = parseSimpleExpression <|> parseParenthesisExpression

-- | Parses an (possibly complex) expression.
parseExpression :: Parser Expression
parseExpression = parseComposeExpression <|> parseParenthesisExpression

-- | Parses a simple expression (i.e. a single word).
parseSimpleExpression :: Parser Expression
parseSimpleExpression = do
  expId <- many1 letter
  return $ ExpId expId

-- | Parses a complex expression (i.e. a function call or an expression between
-- parentheses.
parseComposeExpression :: Parser Expression
parseComposeExpression = do
  liftM ExpList $ (parseSimpleExpression <|> parseParenthesisExpression)
                    `sepBy` try (many1 space)

-- | Parses an expression between parentheses.
parseParenthesisExpression :: Parser Expression
parseParenthesisExpression = do
  char '('
  exp <- parseComposeExpression
  char ')'
  return exp

-- | Parses a comment.
comment :: Parser ()
comment = string "{-" >> manyTill anyChar (try (string "-}")) >> spaces
