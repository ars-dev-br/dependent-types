module DependentTypes.Data
       ( Program (..)
       , TopLevel (..)
       , TypeId (..)
       , emptyProgram
       ) where

import Data.String

data TypeId = TypeId String
            deriving (Show, Eq)

type Signature = [TypeId]

instance IsString TypeId where
  fromString = TypeId

data Program = Program [TopLevel]
             deriving (Show, Eq)

data TopLevel = Type TypeId Signature
              deriving (Show, Eq)

emptyProgram :: Program
emptyProgram = Program []
