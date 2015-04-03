module Semantic.Print
       ( printTests
       ) where

import DependentTypes.Data
import DependentTypes.Parser
import DependentTypes.Semantic
import Semantic.Ext hiding (assertEqual)
import Test.Tasty
import Test.Tasty.HUnit hiding (assertEqual)
import qualified Test.Tasty.HUnit as HUnit (assertEqual)

assertEqual :: String -> String -> IO ()
assertEqual = HUnit.assertEqual ""

printTests = testGroup "Print Statements"
  [ testCase "Printing undefined value" $
    do
      env <- nullEnv
      case parse "print true." of
       Right p -> assertException env p
       Left  e -> assertFailure $ show e

  , testCase "Printing simple constructor" $
    do
      env <- fromList naturals
      case parse "print zero." of
       Right p -> evalProgram (assertEqual "zero.") env p
       Left  e -> assertFailure $ show e

  , testCase "Printing list of simple constructors" $
    do
      env <- fromList $ naturals ++ booleans
      case parse "print zero; true; false." of
       Right p -> evalProgram (assertEqual "zero; true; false.") env p
       Left  e -> assertFailure $ show e

  , testCase "Printing constructor with arguments" $
    do
      env <- fromList naturals
      case parse "print (suc zero)." of
       Right p -> evalProgram (assertEqual "(suc zero).") env p
       Left  e -> assertFailure $ show e

  , testCase "Printing list of constructors with arguments" $
    do
      env <- fromList naturals
      case parse "print (suc zero); (suc (suc zero)); (suc (suc (suc zero)))." of
       Right p -> evalProgram (assertEqual "(suc zero); (suc (suc zero)); (suc (suc (suc zero))).") env p
       Left  e -> assertFailure $ show e

  , testCase "Printing constant" $
    do
      env <- fromList naturals
      case parse "func one : Nat where one = suc zero.\n\
                 \print one." of
       Right p -> evalProgram (assertEqual "(suc zero).") env p
       Left  e -> assertFailure $ show e

  , testCase "Printing function with arguments" $
    do
      env <- fromList naturals
      case parse "func next : Nat -> Nat where\n\
                 \  next x = suc x.\n\
                 \print (next (suc zero))." of
       Right p -> evalProgram (assertEqual "(suc (suc zero)).") env p
       Left  e -> assertFailure $ show e

  , testCase "Printing function with pattern matching" $
    do
      env <- fromList booleans
      case parse "func not : Bool -> Bool where\n\
                 \  not true  = false;\n\
                 \  not false = true.\n\
                 \\n\
                 \print (not true); (not false)." of
       Right p -> evalProgram (assertEqual "false; true.")  env p
       Left  e -> assertFailure $ show e

  , testCase "Printing function with arguments and pattern matching" $
    do
      env <- fromList naturals
      case parse "func add : Nat -> Nat -> Nat where\n\
                 \  add x       zero = x;\n\
                 \  add zero    y    = y;\n\
                 \  add (suc x) y    = suc (add x y).\n\
                 \\n\
                 \print (add (suc zero) (suc (suc zero)))." of
       Right p -> evalProgram (assertEqual "(suc (suc (suc zero))).") env p
       Left  e -> assertFailure $ show e

  , testCase "Printing list of functions" $
    do
      env <- fromList booleans
      case parse "func not : Bool -> Bool where\n\
                 \  not true  = false;\n\
                 \  not false = true.\n\
                 \\n\
                 \print (not true); (not false)." of
       Right p -> evalProgram (assertEqual "false; true.") env p
       Left  e -> assertFailure $ show e

  , testCase "Printing constructor with more arguments" $
    do
      env <- fromList naturals
      case parse "print (suc zero zero)." of
       Right p -> assertException env p
       Left  e -> assertFailure $ show e

  , testCase "Printing constructor with less arguments" $
    do
      env <- fromList naturals
      case parse "print suc." of
       Right p -> assertException env p
       Left  e -> assertFailure $ show e

  , testCase "Printing constructor with wrong type of argument" $
    do
      env <- fromList $ naturals ++ booleans
      case parse "print (suc true)." of
       Right p -> assertException env p
       Left  e -> assertFailure $ show e

  , testCase "Printing constructor with undefined argument" $
    do
      env <- fromList $ naturals
      case parse "print (suc undef)." of
       Right p -> assertException env p
       Left  e -> assertFailure $ show e

  , testCase "Printing mixing constructors and functions" $
    do
      env <- fromList $ naturals
      case parse "func two : Nat where two = suc (suc zero).\n\
                 \print (suc two)." of
       Right p -> evalProgram (assertEqual "(suc (suc (suc zero))).") env p
       Left  e -> assertFailure $ show e

  , testCase "Printing mutually recursive functions" $
    do
      env <- fromList $ naturals ++ booleans
      case parse "func isOdd : Nat -> Bool; isEven : Nat -> Bool where\n\
                 \  isOdd  zero    = false;\n\
                 \  isEven zero    = true;\n\
                 \  isOdd  (suc x) = isEven x;\n\
                 \  isEven (suc x) = isOdd x.\n\
                 \print (isOdd (suc (suc (suc zero)))); (isEven (suc (suc zero)))." of
       Right p -> evalProgram (assertEqual "true; true.") env p
       Left  e -> assertFailure $ show e
  ]
