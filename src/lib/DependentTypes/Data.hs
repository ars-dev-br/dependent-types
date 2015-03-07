module DependentTypes.Data
       ( Program (..)
       , TopLevel (..)
       , emptyProgram
       ) where

data Program = Program [TopLevel]
             deriving (Show, Eq)

data TopLevel = TopLevel
              deriving (Show, Eq)

emptyProgram :: Program
emptyProgram = Program []
