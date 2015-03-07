module Parser where

import Control.Monad
import DependentTypes.Data
import DependentTypes.Parser
import Test.Tasty
import Test.Tasty.HUnit
import Text.Parsec hiding (parse)

parserTests = testGroup "Parser" [emptyProgramTests, typeTests]

emptyProgramTests = testGroup "Empty Programs"
  [ testCase "Empty string" $
      case parse "" of
       Right x -> x @?= emptyProgram
       Left  e -> assertFailure $ show e

  , testCase "Spaces" $
      case parse "    \t   \n  " of
       Right x -> x @?= emptyProgram
       Left  e -> assertFailure $ show e

  , testCase "Single comment" $
      case parse "{- This is a comment. -}" of
       Right x -> x @?= emptyProgram
       Left  e -> assertFailure $ show e

  , testCase "Many comments" $
      case parse "{- First. -}\n\n{- Second. -}" of
       Right x -> x @?= emptyProgram
       Left  e -> assertFailure $ show e
  ]

typeTests = testGroup "Types"
  [ testCase "Empty type" $
      case parse "type Void : Type." of
       Right x -> x @?= (Program [Type "Void" ["Type"]])
       Left  e -> assertFailure $ show e

  , testCase "More empty types" $
      case parse "type Void : Type.\n\ntype Empty : Type." of
       Right x -> x @?= (Program [ Type "Void"  ["Type"]
                                 , Type "Empty" ["Type"]])
       Left  e -> assertFailure $ show e
  ]
