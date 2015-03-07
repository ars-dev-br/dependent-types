import Test.Tasty
import Test.Tasty.HUnit

import Parser

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [parserTests]
