module Semantic.Type
       ( typeTests
       ) where

import DependentTypes.Data
import DependentTypes.Parser
import DependentTypes.Semantic
import Semantic.Ext
import Test.Tasty
import Test.Tasty.HUnit hiding (assertEqual)

typeTests = testGroup "Type Definitions"
  [ testCase "Empty type" $
    do
      env1 <- nullEnv
      env2 <- fromList [("Void", Type "Void" (Signature ["Type"]) [])]

      case parse "type Void : Type." of
       Right p -> evalProgram env1 p
       Left  e -> assertFailure $ show e

      assertEqual env1 env2

  , testCase "Unit type" $
    do
      unit <- return $ Type "Unit" (Signature ["Type"])
                       [Constructor "unit" (Args []) (Signature ["Unit"]) NoConstraint]
      env1 <- nullEnv
      env2 <- fromList [("Unit", unit), ("unit", unit)]

      case parse "type Unit : Type where unit : Unit." of
       Right p -> evalProgram env1 p
       Left  e -> assertFailure $ show e

      assertEqual env1 env2

  , testCase "Type definition not having a type signature" $
    do
      env <- nullEnv
      case parse "type Void : Error." of
       Right p -> assertException env p
       Left  e -> assertFailure $ show e

  , testCase "Type definition depending on undefined type" $
    do
      env <- nullEnv
      case parse "type Error : Nat -> Type." of
       Right p -> assertException env p
       Left  e -> assertFailure $ show e

  , testCase "Type definition depending on defined type" $
    do
      env <- nullEnv
      case parse "type Void : Type. type Valid : Void -> Type." of
       Right p -> evalProgram env p
       Left  e -> assertFailure $ show e

  , testCase "Type constructor depending on defined type" $
    do
      env <- fromList naturals
      case parse "type Bounded : Type where\n\
                 \  lowerBound : Bounded;\n\
                 \  upperBound : Bounded;\n\
                 \  bounded    : Nat -> Bounded." of
       Right p -> evalProgram env p
       Left  e -> assertFailure $ show e

  , testCase "Type constructor depending on undefined type" $
    do
      env <- nullEnv
      case parse "type Bounded : Type where\n\
                 \  lowerBound : Bounded;\n\
                 \  upperBound : Bounded;\n\
                 \  bounded    : Nat -> Bounded." of
       Right p -> assertException env p
       Left  e -> assertFailure $ show e

  ]
