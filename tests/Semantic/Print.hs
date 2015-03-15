module Semantic.Print
       ( printTests
       ) where

import DependentTypes.Data
import DependentTypes.Parser
import DependentTypes.Semantic
import Semantic.Ext
import Test.Tasty
import Test.Tasty.HUnit hiding (assertEqual)

printTests = testGroup "Print Statements"
  [ testCase "Printing undefined value" $
    do
      env <- nullEnv
      case parse "print true." of
       Right p -> assertException env p
       Left  e -> assertFailure $ show e

  , testCase "Printing simple constructor" $
    do
      env <- fromList [("Nat", nat)]
      case parse "print zero." of
       Right p -> evalProgram env p
       Left  e -> assertFailure $ show e

  , testCase "Printing list of simple constructors" $
    do
      env <- fromList [("Nat", nat), ("Bool", bool)]
      case parse "print zero; true; false." of
       Right p -> evalProgram env p
       Left  e -> assertFailure $ show e

  , testCase "Printing constant" $
    do
      env <- fromList [("Nat", nat)]
      case parse "func one : Nat where one = suc zero.\n\
                 \print one." of
       Right p -> evalProgram env p
       Left  e -> assertFailure $ show e

  , testCase "Printing function" $
    do
      env <- fromList [("Bool", bool)]
      case parse "func not : Bool -> Bool where\n\
                 \  not true  = false;\n\
                 \  not false = true.\n\
                 \\n\
                 \print (not true)." of
       Right p -> evalProgram env p
       Left  e -> assertFailure $ show e

  , testCase "Printing list functions" $
    do
      env <- fromList [("Bool", bool)]
      case parse "func not : Bool -> Bool where\n\
                 \  not true  = false;\n\
                 \  not false = true.\n\
                 \\n\
                 \print (not true); (not false)." of
       Right p -> evalProgram env p
       Left  e -> assertFailure $ show e
  ]
