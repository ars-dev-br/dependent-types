{-|
Module:      DependentTypes.Data
Description: Data types used by the library.
-}

module DependentTypes.Data
       ( Args (..)
       , Constructor (..)
       , Expression (..)
       , Id (..)
       , Lambda (..)
       , Program (..)
       , Signature (..)
       , Toplevel (..)
       , TypeId (..)
       , emptyProgram
       ) where

import Data.String

-- | The name of an identifier.
newtype Id = Id String
           deriving (Show, Eq)

instance IsString Id where
  fromString = Id

-- | The name of a type.
newtype TypeId = TypeId String
               deriving (Show, Eq)

instance IsString TypeId where
  fromString = TypeId

-- | List of type names, indicating the type signature of a function or a type.
newtype Signature = Signature [TypeId]
                  deriving (Show, Eq)

-- | A program, with all its toplevel constructs.
data Program = Program [Toplevel]
             deriving (Show, Eq)

-- | Toplevel constructs.
data Toplevel = Type TypeId Signature [Constructor]  -- ^ A type declaration.
              | Func Id Signature [Lambda]           -- ^ A function declaration.
              | Print                                -- ^ A print declaration.
              deriving (Show, Eq)

-- | A type constructor.
data Constructor = Constructor Id Signature
                 deriving (Show, Eq)

-- | Each declaration body of a function
data Lambda = Lambda Args Expression
            deriving (Show, Eq)

newtype Args = Args [Expression]
                  deriving (Show, Eq)

data Expression = ExpId Id
                | ExpList [Expression]
                deriving (Show, Eq)

-- | Returns an empty Program.
emptyProgram :: Program
emptyProgram = Program []
