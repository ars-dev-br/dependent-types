{-|
Module:      DependentTypes.Data
Description: Data types used by the library.
-}

module DependentTypes.Data
       ( Args (..)
       , Constraint (..)
       , Constructor (..)
       , Expression (..)
       , Lambda (..)
       , Program (..)
       , Signature (..)
       , Toplevel (..)
       , TypeDef (..)
       , emptyProgram
       , typeName
       ) where

import Data.String

-- | The definition of a type.
data TypeDef = TypeId String                 -- ^ A simple type declaration.
             | DepType String [Expression]   -- ^ A dependent type declaration.
             deriving (Show, Eq)

instance IsString TypeDef where
  fromString = TypeId

-- | List of type names, indicating the type signature of a function or a type.
newtype Signature = Signature [TypeDef]
                  deriving (Show, Eq)

-- | A program, with all its toplevel constructs.
data Program = Program [Toplevel]
             deriving (Show, Eq)

-- | Toplevel constructs.
data Toplevel = Type String Signature [Constructor]       -- ^ A type declaration.
              | Func [(String, Signature)] [Lambda]       -- ^ A function declaration.
              | Print [Expression]                        -- ^ A print declaration.
              | Var Expression                            -- ^ Internally used for variable bindings
              deriving (Show, Eq)

-- | A type constructor.
data Constructor = Constructor String Args Signature Constraint
                 deriving (Show, Eq)

-- | Each declaration body of a function
data Lambda = Lambda String Args Expression
            deriving (Show, Eq)

-- | Arguments used by a function or a high-kinded type.
newtype Args = Args [Expression]
                  deriving (Show, Eq)

-- | Expressions to be executed.
data Expression = ExpId String
                | ExpList [Expression]
                deriving (Eq)

instance Show Expression where
  show (ExpId expId) = expId
  show (ExpList expList) = "(" ++ (unwords $ map show expList) ++ ")"

-- | Type constructor constraint.
data Constraint = Constraint Expression  -- ^ A constructor contrained by this expression.
                | NoConstraint           -- ^ A constructor with no constraints.
                deriving (Show, Eq)

-- | Returns an empty Program.
emptyProgram :: Program
emptyProgram = Program []

-- | Returns the name of a type
typeName :: TypeDef -> String
typeName (TypeId name) = name
typeName (DepType name _) = name
