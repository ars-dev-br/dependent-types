module Semantic where

import Control.Exception
import Data.IORef
import Data.Map (Map)
import DependentTypes.Data
import DependentTypes.Parser
import DependentTypes.Semantic
import Test.Tasty
import Test.Tasty.HUnit hiding (assertEqual)
import qualified Test.Tasty.HUnit as HUnit (assertEqual)

assertEqual :: IORef (Map String Toplevel) -> IORef (Map String Toplevel) -> IO ()
assertEqual envRef1 envRef2 = do
  env1 <- readIORef envRef1
  env2 <- readIORef envRef2
  HUnit.assertEqual "" env1 env2

assertException :: Env -> Program -> IO ()
assertException env p = do
  except <- try $ evalProgram env p :: IO (Either ErrorCall ())
  case except of
   Right p -> assertFailure "Invalid program was accepted."
   Left  e -> return ()

nat :: Toplevel
nat = Type "Nat" (Signature ["Type"]) [ Constructor "zero" (Args []) (Signature ["Nat"])
                                        NoConstraint
                                      , Constructor "suc" (Args []) (Signature ["Nat", "Nat"])
                                        NoConstraint ]

bool :: Toplevel
bool = Type "Bool" (Signature ["Type"]) [ Constructor "false" (Args []) (Signature ["Bool"])
                                          NoConstraint
                                        , Constructor "true" (Args []) (Signature ["Bool"])
                                          NoConstraint ]

semanticTests = testGroup "Semantic" [emptyTests, typeTests, funcTests, printTests]

emptyTests = testGroup "Empty Program"
  [ testCase "Empty Program" $
    do
      env1 <- nullEnv
      env2 <- nullEnv
      evalProgram env1 $ Program []

      assertEqual env1 env2
  ]

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
      env1 <- nullEnv
      env2 <- fromList [("Unit", Type "Unit" (Signature ["Type"])
                                      [Constructor "unit" (Args []) (Signature ["Unit"]) NoConstraint])]

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
  ]

funcTests = testGroup "Function Definitions"
  [ testCase "Simple function" $
    do
      env1 <- nullEnv
      env2 <- fromList [ ("Nat", nat)
                       , ("one", Func [("one", Signature ["Nat"])]
                                 [Lambda "one" (Args []) (ExpList [ ExpId "suc", ExpId "zero" ])]) ]

      case parse "type Nat : Type where\n\
                 \  zero : Nat;\n\
                 \  suc  : Nat -> Nat.\n\
                 \\n\
                 \func one : Nat where\n\
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

      env1 <- nullEnv
      env2 <- fromList [ ("Nat", nat), ("Bool", bool)
                       , ("odd", Func [("odd", Signature ["Nat", "Bool"])] oddEven)
                       , ("even", Func [("even", Signature ["Nat", "Bool"])] oddEven) ]

      case parse "type Nat : Type where\n\
                 \  zero : Nat;\n\
                 \  suc  : Nat -> Nat.\n\
                 \\n\
                 \type Bool : Type where\n\
                 \  false : Bool;\n\
                 \  true  : Bool.\n\
                 \\n\
                 \func odd : Nat -> Bool; even : Nat -> Bool where\n\
                 \  odd  zero    = false;\n\
                 \  even zero    = true;\n\
                 \  odd (suc x)  = even x;\n\
                 \  even (suc x) = odd x." of
       Right p -> evalProgram env1 p
       Left  e -> assertFailure $ show e

      assertEqual env1 env2

  ,  testCase "Function definition for undefined type" $
     do
       env <- nullEnv
       case parse "func not : Bool -> Bool where not true = false; not false = true." of
        Right p -> assertException env p
        Left  e -> assertFailure $ show e
  ]

printTests = testGroup "Print Statements"
  [ testCase "Printing undefined value" $
    do
      env <- nullEnv
      evalProgram env $ Program [Print (ExpList [ExpId "true"])]
  ]
