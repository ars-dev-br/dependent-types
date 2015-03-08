module Parser where

import Control.Monad
import DependentTypes.Data
import DependentTypes.Parser
import Test.Tasty
import Test.Tasty.HUnit
import Text.Parsec hiding (parse)

parserTests = testGroup "Parser" [emptyProgramTests, typeTests, funcTests, printTests]

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
  [ testCase "Constant" $
      case parse "func one : Nat where one = suc zero." of
       Right x -> x @?= (Program [ Func "one" (Signature ["Nat"])
                                        [ Lambda (Args []) (ExpList [ExpId "suc", ExpId "zero"]) ] ])
       Left  e -> assertFailure $ show e

  , testCase "Simple pattern matching" $
      case parse "func not : Bool -> Bool where\n\
                 \  not true  = false;\n\
                 \  not false = true." of
       Right x -> x @?= (Program [ Func "not" (Signature ["Bool", "Bool"])
                                        [ Lambda (Args [ExpId "true"])  (ExpList [ExpId "false"])
                                        , Lambda (Args [ExpId "false"]) (ExpList [ExpId "true"]) ] ])
       Left  e -> assertFailure $ show e

  , testCase "Complex expression" $
      case parse "func three : Nat where\n\
                 \  three = suc (suc (suc zero))." of
       Right x -> x @?= (Program [ Func "three" (Signature ["Nat"])
                                        [ Lambda (Args [])
         (ExpList [ExpId "suc", (ExpList [ExpId "suc", (ExpList [ExpId "suc", ExpId "zero"])])]) ] ])
       Left  e -> assertFailure $ show e

  , testCase "Function with two arguments" $
      case parse "func add : Nat -> Nat -> Nat where\n\
                 \  add x       zero = x;\n\
                 \  add zero    y    = y;\n\
                 \  add (suc x) y    = suc (add x y)." of
       Right x -> x @?= (Program [ Func "add" (Signature ["Nat", "Nat", "Nat"])
                                        [ Lambda (Args [ExpId "x", ExpId "zero"]) (ExpList [ExpId "x"])
                                        , Lambda (Args [ExpId "zero", ExpId "y"]) (ExpList [ExpId "y"])
                                        , Lambda (Args [ExpList [ExpId "suc", ExpId "x"], ExpId "y"])
                                                 (ExpList [ExpId "suc", ExpList [ ExpId "add"
                                                                                , ExpId "x"
                                                                                , ExpId "y" ]])] ])
       Left  e -> assertFailure $ show e
  ]

printTests = testGroup "Print"
  [ testCase "Single expression" $
      case parse "print zero." of
       Right x -> x @?= (Program [ Print (ExpList [ExpId "zero"]) ])
       Left  e -> assertFailure $ show e

  , testCase "Complex expression" $
      case parse "print (not true); six; seven." of
       Right x -> x @?= (Program [ Print (ExpList [ ExpList [ExpId "not", ExpId "true"]
                                                  , ExpId "six"
                                                  , ExpId "seven" ])])
       Left  e -> assertFailure $ show e
  ]
