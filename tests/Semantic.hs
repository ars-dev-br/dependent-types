module Semantic
       ( semanticTests
       ) where

import Control.Exception
import Data.IORef
import Data.Map (Map)
import DependentTypes.Data
import DependentTypes.Parser
import DependentTypes.Semantic
import Semantic.DepType
import Semantic.Ext
import Semantic.Func
import Semantic.Print
import Semantic.Type
import Test.Tasty
import Test.Tasty.HUnit hiding (assertEqual)
import qualified Test.Tasty.HUnit as HUnit (assertEqual)

semanticTests = testGroup "Semantic" [emptyTests, typeTests, depTypeTests, funcTests, printTests]

emptyTests = testGroup "Empty Program"
  [ testCase "Empty Program" $
    do
      env1 <- nullEnv
      env2 <- nullEnv
      evalQuietProgram env1 $ Program []

      assertEqual env1 env2
  ]
