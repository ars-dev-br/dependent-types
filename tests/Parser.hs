module Parser where

import Control.Monad
import DependentTypes.Data
import DependentTypes.Parser
import Test.Tasty
import Test.Tasty.HUnit

parserTests = testGroup "Parser" [emptyProgramTests]

emptyProgramTests = testGroup "Empty Programs"
  [ testCase "Empty string" $
      case parse "" of
       Right x -> x @?= emptyProgram
       Left  _ -> assertFailure ""

  , testCase "Spaces" $
      case parse "    \t   \n  " of
       Right x -> x @?= emptyProgram
       Left  _ -> assertFailure ""

  , testCase "Single comment" $
      case parse "{- This is a comment. -}" of
       Right x -> x @?= emptyProgram
       Left  _ -> assertFailure ""

  , testCase "Many comments" $
      case parse "{- First. -}\n\n{- Second. -}" of
       Right x -> x @?= emptyProgram
       Left  _ -> assertFailure ""
 ]
