module Parser where

import Control.Monad
import DependentTypes.Data
import DependentTypes.Parser
import Test.Tasty
import Test.Tasty.HUnit
import Text.Parsec hiding (parse)

parserTests = testGroup "Parser" [emptyProgramTests, typeTests, funcTests]

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
       Right x -> x @?= (Program [Type "Void" (Signature ["Type"]) []])
       Left  e -> assertFailure $ show e

  , testCase "More empty types" $
      case parse "type Void : Type.\n\ntype Empty : Type." of
       Right x -> x @?= (Program [ Type "Void"  (Signature ["Type"]) []
                                 , Type "Empty" (Signature ["Type"]) [] ])
       Left  e -> assertFailure $ show e

  , testCase "Types with comments between" $
      case parse "type Void : Type. \n\
                 \ \n\
                 \{- Comment. -}\n\
                 \type Empty : Type.\n" of
       Right x -> x @?= (Program [ Type "Void"  (Signature ["Type"]) []
                                 , Type "Empty" (Signature ["Type"]) [] ])
       Left  e -> assertFailure $ show e

  , testCase "Type with comment before" $
      case parse "{- Comment. -} \n\
                 \type Void : Type. \n" of
        Right x -> x @?= (Program [Type "Void" (Signature ["Type"]) []])
        Left  e -> assertFailure $ show e

  , testCase "Types with many comments between" $
      case parse "type Void : Type. \n\
                 \ \n\
                 \{- Comment 1. -}\n\
                 \{- Comment 2. -}\n\
                 \type Empty : Type.\n" of
      Right x -> x @?= (Program [ Type "Void"  (Signature ["Type"]) []
                                , Type "Empty" (Signature ["Type"]) [] ])
      Left  e -> assertFailure $ show e

  , testCase "Unit type" $
      case parse "type Unit : Type where\n\
                 \  unit : Unit." of
       Right x -> x @?= (Program [ Type "Unit" (Signature ["Type"])
                                        [ Constructor "unit" (Signature ["Unit"]) ]])
       Left  e -> assertFailure $ show e

  , testCase "Boolean type" $
      case parse "type Bool : Type where\n\
                 \  false : Bool;\n\
                 \  true  : Bool." of
       Right x -> x @?= (Program [ Type "Bool" (Signature ["Type"])
                                        [ Constructor "false" (Signature ["Bool"])
                                        , Constructor "true"  (Signature ["Bool"]) ]])
       Left  e -> assertFailure $ show e

  , testCase "Natural type" $
      case parse "type Nat : Type where\n\
                 \  zero : Nat;\n\
                 \  suc  : Nat -> Nat." of
       Right x -> x @?= (Program [ Type "Nat" (Signature ["Type"])
                                        [ Constructor "zero" (Signature ["Nat"])
                                        , Constructor "suc"  (Signature ["Nat", "Nat"]) ]])
       Left  e -> assertFailure $ show e
  ]

funcTests = testGroup "Functions"
  [ {- testCase "Constant" $
      case parse "func one : Nat where one = suc zero." of
       Right x -> x @?= (Program [Func "one" (Signature ["Nat"])])
       Left  e -> assertFailure $ show e -}
  ]
