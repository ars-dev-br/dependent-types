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
  spaces
  skipMany comment
  spaces
  statements <- parseToplevel `endBy` toplevelDelimiter
  eof
  return $ Program statements
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
  signatures <- parseSignatures
  spaces
  string "where"
  lambdas <- parseLambdas
  return $ Func signatures lambdas

-- | Parses a print declaration.
parsePrint :: Parser Toplevel
parsePrint = do
  string "print"
  spaces
  exp <- parsePrintExpressions
  return $ Print exp

-- | Parses the expressions for a print statement.
parsePrintExpressions :: Parser [Expression]
parsePrintExpressions = parseArg `sepBy` printDelimiter
  where
    printDelimiter = (spaces >> char ';' >> spaces)

-- | Parses the signature of at least one function.
parseSignatures :: Parser [(String, Signature)]
parseSignatures = parseFuncSignature `sepBy1` funcSignatureDelimiter
  where
    funcSignatureDelimiter = try (spaces >> char ';' >> spaces)

-- | Parses the signature of a single function
parseFuncSignature :: Parser (String, Signature)
parseFuncSignature = do
  spaces
  funcId <- many1 letter
  spaces
  char ':'
  spaces
  signature <- parseSignature
  return $ (funcId, signature)

-- | Parses a type signature.
parseSignature :: Parser Signature
parseSignature = do
  types <- parseTypeDef `sepBy1` signatureDelimiter
  return $ Signature types
    where
      signatureDelimiter = try (spaces >> string "->" >> spaces)

parseTypeDef :: Parser TypeDef
parseTypeDef = parseTypeId <|> parseDepType

-- | Parses a type
parseTypeId :: Parser TypeDef
parseTypeId = liftM TypeId $ many1 letter

-- | Parses a dependent type
parseDepType :: Parser TypeDef
parseDepType = do
  char '('
  typeId <- many1 letter
  (Args args) <- parseArgs
  char ')'
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
  args <- parseArgs
  char ':'
  spaces
  signature <- parseSignature
  constraint <- parseConstraint
  return $ Constructor constructorId args signature constraint

-- | Parses a constraint for a type constructor.
parseConstraint :: Parser Constraint
parseConstraint = option NoConstraint parseConstraint'
  where
    parseConstraint' = do
      spaces
      char '|'
      spaces
      liftM Constraint $ parseExpression

-- | Parses all declaration bodies of a function.
parseLambdas :: Parser [Lambda]
parseLambdas = parseLambda `sepBy1` lambdaDelimiter
  where
    lambdaDelimiter = try (spaces >> char ';' >> spaces)

-- | Parses each declaration body of a function
parseLambda :: Parser Lambda
parseLambda = do
  spaces
  func <- many1 letter
  args <- parseArgs
  char '='
  spaces
  exp <- parseExpression
  return $ Lambda func args exp

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
