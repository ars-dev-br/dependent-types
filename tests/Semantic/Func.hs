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
      env1 <- fromList [ ("Nat", nat) ]
      env2 <- fromList [ ("Nat", nat)
                       , ("one", Func [("one", Signature ["Nat"])]
                                 [Lambda "one" (Args []) (ExpList [ ExpId "suc", ExpId "zero" ])]) ]

      case parse "func one : Nat where\n\
                 \  one = suc zero." of
       Right p -> evalProgram env1 p
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

      env1 <- fromList [ ("Nat", nat), ("Bool", bool) ]
      env2 <- fromList [ ("Nat", nat), ("Bool", bool)
                       , ("odd", Func [("odd", Signature ["Nat", "Bool"])] oddEven)
                       , ("even", Func [("even", Signature ["Nat", "Bool"])] oddEven) ]

      case parse "func odd : Nat -> Bool; even : Nat -> Bool where\n\
                 \  odd  zero    = false;\n\
                 \  even zero    = true;\n\
                 \  odd (suc x)  = even x;\n\
                 \  even (suc x) = odd x." of
       Right p -> evalProgram env1 p
       Left  e -> assertFailure $ show e

      assertEqual env1 env2

  , testCase "Function definition for undefined type" $
    do
      env <- nullEnv
      case parse "func not : Bool -> Bool where not true = false; not false = true." of
       Right p -> assertException env p
       Left  e -> assertFailure $ show e

  ]
