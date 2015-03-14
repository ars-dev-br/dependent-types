module Semantic where

import Control.Exception
import Data.IORef
import Data.Map (Map)
import DependentTypes.Data
import DependentTypes.Semantic
import Test.Tasty
import Test.Tasty.HUnit hiding (assertEqual)
import qualified Test.Tasty.HUnit as HUnit (assertEqual)

assertEqual :: IORef (Map String Toplevel) -> IORef (Map String Toplevel) -> IO ()
assertEqual envRef1 envRef2 = do
  env1 <- readIORef envRef1
  env2 <- readIORef envRef2
  HUnit.assertEqual "" env1 env2

testSimpleEval :: String -> String -> Toplevel -> TestTree
testSimpleEval title id value = testCase title $
                                do
                                  env1 <- nullEnv
                                  env2 <- fromList [(id, value)]

                                  evalProgram env1 $ Program [value]
                                  assertEqual env1 env2


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
  [ testSimpleEval "Empty type" "Void" $ Type "Void" (Signature ["Type"]) []
  ]

funcTests = testGroup "Function Definitions"
  [ testSimpleEval "Simple function" "one" $ Func [("one", Signature ["Nat"])]
                                                  [Lambda "one" (Args []) (ExpList [ ExpId "suc"
                                                                                   , ExpId "zero"])]

  , testCase "Mutually recursive function" $
    do
      env1 <- nullEnv
      env2 <- fromList []
      evalProgram env1 $ Program []

      assertEqual env1 env2
  ]

printTests = testGroup "Print Statements"
  [ testCase "Printing undefined value" $
    do
      env <- nullEnv
      evalProgram env $ Program [Print (ExpList [ExpId "true"])]
  ]
