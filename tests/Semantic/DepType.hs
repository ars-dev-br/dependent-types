module Semantic.DepType
       ( depTypeTests
       ) where

import DependentTypes.Data
import DependentTypes.Parser
import DependentTypes.Semantic
import Semantic.Ext
import Test.Tasty
import Test.Tasty.HUnit hiding (assertEqual)

depTypeTests = testGroup "Dependent Types"
  [ testCase "" $
    do
      return ()
  ]
