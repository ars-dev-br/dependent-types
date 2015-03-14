import Test.Tasty
import Test.Tasty.HUnit

import Parser
import Semantic

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [parserTests, semanticTests]
