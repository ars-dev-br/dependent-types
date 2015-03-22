module Semantic.Func
       ( funcTests
       ) where

import DependentTypes.Data
import DependentTypes.Parser
import DependentTypes.Semantic
import Semantic.Ext
import Test.Tasty
import Test.Tasty.HUnit hiding (assertEqual)

funcTests = testGroup "Function Definitions"
  [ testCase "Simple function" $
    do
      env1 <- fromList naturals
      env2 <- fromList $ naturals ++
                         [ ("one", Func [("one", Signature ["Nat"])]
                                   [Lambda "one" (Args []) (ExpList [ ExpId "suc", ExpId "zero" ])]) ]

      case parse "func one : Nat where\n\
                 \  one = suc zero." of
       Right p -> evalQuietProgram env1 p
       Left  e -> assertFailure $ show e

      assertEqual env1 env2

  , testCase "Mutually recursive function" $
    do
      oddEven <- return $ [ Lambda "odd" (Args [ExpId "zero"]) (ExpList [ExpId "false"])
                          , Lambda "even" (Args [ExpId "zero"]) (ExpList [ExpId "true"])
                          , Lambda "odd" (Args [ExpList [ExpId "suc", ExpId "x"]])
                                   (ExpList [ExpId "even", ExpId "x"])
                          , Lambda "even" (Args [ExpList [ExpId "suc", ExpId "x"]])
                                   (ExpList [ExpId "odd", ExpId "x"]) ]

      env1 <- fromList $ naturals ++ booleans
      env2 <- fromList $ naturals ++ booleans ++
                       [ ("odd", Func [("odd", Signature ["Nat", "Bool"])] oddEven)
                       , ("even", Func [("even", Signature ["Nat", "Bool"])] oddEven) ]

      case parse "func odd : Nat -> Bool; even : Nat -> Bool where\n\
                 \  odd  zero    = false;\n\
                 \  even zero    = true;\n\
                 \  odd (suc x)  = even x;\n\
                 \  even (suc x) = odd x." of
       Right p -> evalQuietProgram env1 p
       Left  e -> assertFailure $ show e

      assertEqual env1 env2

  , testCase "Function with undefined type" $
    do
      env <- nullEnv
      case parse "func not : Bool -> Bool where not true = false; not false = true." of
       Right p -> assertException env p
       Left  e -> assertFailure $ show e

  , testCase "Function using undefined symbol" $
    do
      env <- fromList naturals
      case parse "func two : Nat where two = add (suc zero) (suc zero)." of
       Right p -> assertException env p
       Left  e -> assertFailure $ show e

  , testCase "Function using more arguments for function call" $
    do
      env <- fromList naturals
      case parse "func addOne : Nat -> Nat where addOne x = suc x.\n\
                 \func invalid : Nat -> Nat where invalid x = addOne x x." of
       Right p -> assertException env p
       Left  e -> assertFailure $ show e

  , testCase "Function using less arguments for function call" $
    do
      env <- fromList naturals
      case parse "func addOne : Nat -> Nat where addOne x = suc x.\n\
                 \func invalid : Nat -> Nat where invalid x = addOne." of
       Right p -> assertException env p
       Left  e -> assertFailure $ show e

  , testCase "Function using more arguments for constructor" $
    do
      env <- fromList naturals
      case parse "func addOne : Nat -> Nat where addOne x = suc x x." of
       Right p -> assertException env p
       Left  e -> assertFailure $ show e

  , testCase "Function using less arguments for constructor" $
    do
      env <- fromList naturals
      case parse "func addOne : Nat -> Nat where addOne x = suc." of
       Right p -> assertException env p
       Left  e -> assertFailure $ show e

  , testCase "Function using wrong type of argument for function call" $
    do
      env <- fromList $ naturals ++ booleans
      case parse "func addOne : Nat -> Nat where addOne x = suc x.\n\
                 \func invalid : Nat where invalid = addOne true." of
       Right p -> assertException env p
       Left  e -> assertFailure $ show e

  , testCase "Function using wrong type of argument for constructor" $
    do
      env <- fromList $ naturals ++ booleans
      case parse "func invalid : Nat where invalid = suc true." of
       Right p -> assertException env p
       Left  e -> assertFailure $ show e

  , testCase "Function not matching with signature" $
    do
      env <- fromList $ naturals
      case parse "func invalid : Nat where foo = zero." of
       Right p -> assertException env p
       Left  e -> assertFailure $ show e

  , testCase "Mutually function not matching with signature" $
    do
      env <- fromList $ naturals
      case parse "func invalid : Nat; alsoInvalid : Nat where\n\
                 \  foo = zero." of
       Right p -> assertException env p
       Left  e -> assertFailure $ show e

  , testCase "Function definition with wrong number of arguments" $
    do
      env <- fromList $ booleans
      case parse "func and : Bool -> Bool -> Bool where\n\
                 \  and a b c = false." of
       Right p -> assertException env p
       Left  e -> assertFailure $ show e

  , testCase "Function which returns a parameterless constructor" $
    do
     env <- fromList $ naturals
     case parse "func newZero : Nat where newZero = zero." of
      Right p -> evalQuietProgram env p
      Left  e -> assertFailure $ show e

  , testCase "Function which calls a parameterless function" $
    do
      env <- fromList $ naturals
      case parse "func newZero : Nat where newZero = zero.\n\
                 \func otherZero : Nat where otherZero = newZero." of
       Right p -> evalQuietProgram env p
       Left  e -> assertFailure $ show e

  , testCase "Function which calls a paramaterless undefined function" $
    do
      env <- fromList $ naturals
      case parse "func otherZero : Nat where otherZero = newZero." of
       Right p -> assertException env p
       Left  e -> assertFailure $ show e
  ]
