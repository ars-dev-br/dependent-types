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
                                        [ Constructor "unit" (Args []) (Signature ["Unit"]) NoConstraint ]])
       Left  e -> assertFailure $ show e

  , testCase "Boolean type" $
      case parse "type Bool : Type where\n\
                 \  false : Bool;\n\
                 \  true  : Bool." of
       Right x -> x @?= (Program [ Type "Bool" (Signature ["Type"])
                                        [ Constructor "false" (Args []) (Signature ["Bool"]) NoConstraint
                                        , Constructor "true"  (Args []) (Signature ["Bool"]) NoConstraint ]])
       Left  e -> assertFailure $ show e

  , testCase "Natural type" $
      case parse "type Nat : Type where\n\
                 \  zero : Nat;\n\
                 \  suc  : Nat -> Nat." of
       Right x -> x @?= (Program [ Type "Nat" (Signature ["Type"])
                                        [ Constructor "zero" (Args []) (Signature ["Nat"]) NoConstraint
                                        , Constructor "suc"  (Args []) (Signature ["Nat", "Nat"])
                                                      NoConstraint ]])
       Left  e -> assertFailure $ show e

  , testCase "Type with parameters" $
      case parse "type List : Nat -> Type -> Type where\n\
                 \  nil  : List zero a;\n\
                 \  cons : a -> List n a -> List (suc n) a." of
       Right x -> x @?= (Program [ Type "List" (Signature ["Nat", "Type", "Type"])
                                        [ Constructor "nil"
                                                      (Args [])
                                                      (Signature [DepType "List"
                                                                          [ExpId "zero", ExpId "a"]])
                                                      NoConstraint

                                        , Constructor "cons"
                                                      (Args [])
                                                      (Signature [ "a"
                                                                 , DepType "List"
                                                                           [ExpId "n", ExpId "a"]
                                                                 , DepType "List"
                                                                           [ ExpList [ExpId "suc", ExpId "n"]
                                                                           , ExpId "a" ]])
                                                      NoConstraint]])
       Left  e -> assertFailure $ show e

  , testCase "Constructors with arguments" $
      case parse "data LessOrEqual : Nat -> Nat -> Type where\n\
                 \  lessZero zero    y       : LessOrEqual zero y;\n\
                 \  lessSuc  (suc x) (suc y) : LessOrEqual (suc x) (suc y)." of
       Right x -> x @?= (Program [ Type "LessOrEqual" (Signature ["Nat", "Nat", "Type"])
                                        [ Constructor "lessZero"
                                                      (Args [ExpId "zero", ExpId "y"])
                                                      (Signature [DepType "LessOrEqual"
                                                                          [ExpId "zero", ExpId "y"]])
                                                      NoConstraint
                                        , Constructor "lessSuc"
                                                      (Args [ ExpList [ExpId "suc", ExpId "x"]
                                                            , ExpList [ExpId "suc", ExpId "y"] ])
                                                      (Signature [DepType "LessOrEqual"
                                                                          [ ExpList [ExpId "suc", ExpId "x"]
                                                                          , ExpList [ExpId "suc", ExpId "y"]]])
                                                      NoConstraint ]])
       Left  e -> assertFailure $ show e

  , testCase "Types with restrictions" $
      case parse "data LessOrEqual : Nat -> Nat -> Type where\n\
                 \  lessZero zero    y       : LessOrEqual zero y;\n\
                 \  lessSuc  (suc x) (suc y) : LessOrEqual (suc x) (suc y) | LessOrEqual x y." of
       Right x -> x @?= (Program [ Type "LessOrEqual" (Signature ["Nat", "Nat", "Type"])
                                        [ Constructor "lessZero"
                                                      (Args [ExpId "zero", ExpId "y"])
                                                      (Signature [DepType "LessOrEqual"
                                                                          [ExpId "zero", ExpId "y"]])
                                                      NoConstraint
                                        , Constructor "lessSuc"
                                                      (Args [ ExpList [ExpId "suc", ExpId "x"]
                                                            , ExpList [ExpId "suc", ExpId "y"] ])
                                                      (Signature [DepType "LessOrEqual"
                                                                          [ ExpList [ExpId "suc", ExpId "x"]
                                                                          , ExpList [ExpId "suc", ExpId "y"]]])
                                                      (Constraint (ExpList [ ExpId "LessOrEqual"
                                                                           , ExpId "x"
                                                                           , ExpId "y" ] )) ]])
       Left  e -> assertFailure $ show e
  ]

funcTests = testGroup "Functions"
  [ testCase "Constant" $
      case parse "func one : Nat where one = suc zero." of
       Right x -> x @?= (Program [ Func [("one", Signature ["Nat"])]
                                        [ Lambda "one" (Args []) (ExpList [ExpId "suc", ExpId "zero"]) ] ])
       Left  e -> assertFailure $ show e

  , testCase "Simple pattern matching" $
      case parse "func not : Bool -> Bool where\n\
                 \  not true  = false;\n\
                 \  not false = true." of
       Right x -> x @?= (Program [ Func [("not", Signature ["Bool", "Bool"])]
                                        [ Lambda "not" (Args [ExpId "true"])  (ExpList [ExpId "false"])
                                        , Lambda "not" (Args [ExpId "false"]) (ExpList [ExpId "true"]) ] ])
       Left  e -> assertFailure $ show e

  , testCase "Complex expression" $
      case parse "func three : Nat where\n\
                 \  three = suc (suc (suc zero))." of
       Right x -> x @?= (Program [ Func [("three", Signature ["Nat"])]
                                        [ Lambda "three" (Args [])
         (ExpList [ExpId "suc", (ExpList [ExpId "suc", (ExpList [ExpId "suc", ExpId "zero"])])]) ] ])
       Left  e -> assertFailure $ show e

  , testCase "Function with two arguments" $
      case parse "func add : Nat -> Nat -> Nat where\n\
                 \  add x       zero = x;\n\
                 \  add zero    y    = y;\n\
                 \  add (suc x) y    = suc (add x y)." of
       Right x -> x @?= (Program [ Func [("add", Signature ["Nat", "Nat", "Nat"])]
                                        [ Lambda "add" (Args [ExpId "x", ExpId "zero"]) (ExpList [ExpId "x"])
                                        , Lambda "add" (Args [ExpId "zero", ExpId "y"]) (ExpList [ExpId "y"])
                                        , Lambda "add" (Args [ExpList [ExpId "suc", ExpId "x"], ExpId "y"])
                                                 (ExpList [ExpId "suc", ExpList [ ExpId "add"
                                                                                , ExpId "x"
                                                                                , ExpId "y" ]])] ])
       Left  e -> assertFailure $ show e

  , testCase "Function with dependent types" $
      case parse "func sumOfOdd : Odd n -> Odd m -> Even (add n m) where\n\
                 \  sumOfOdd oddOne     oddOne     = evenSuc evenZero;\n\
                 \  sumOfOdd oddOne     (oddSuc y) = evenSuc (sumOfOdd oddOne y);\n\
                 \  sumOfOdd (oddSuc x) y          = evenSuc (sumOfOdd x y)." of
       Right x -> x @?= (Program [ Func [("sumOfOdd", Signature [ DepType "Odd" [ExpId "n"]
                                                                , DepType "Odd" [ExpId "m"]
                                                                , DepType "Even" [ ExpList [ ExpId "add"
                                                                                           , ExpId "n"
                                                                                           , ExpId "m" ]]])]
                                                     [ Lambda "sumOfOdd" (Args [ExpId "oddOne", ExpId "oddOne"])
                                                              (ExpList [ExpId "evenSuc", ExpId "evenZero"])
                                                     , Lambda "sumOfOdd" (Args [ExpId "oddOne", ExpList [ ExpId "oddSuc"
                                                                                             , ExpId "y" ]])
                                                              (ExpList [ ExpId "evenSuc"
                                                                       , ExpList [ ExpId "sumOfOdd"
                                                                                 , ExpId "oddOne"
                                                                                 , ExpId "y" ]])
                                                     , Lambda "sumOfOdd" (Args [ ExpList [ ExpId "oddSuc"
                                                                              , ExpId "x" ]
                                                                    , ExpId "y" ])
                                                              (ExpList [ ExpId "evenSuc"
                                                                       , ExpList [ ExpId "sumOfOdd"
                                                                                 , ExpId "x"
                                                                                 , ExpId "y" ]])]])
       Left  e -> assertFailure $ show e

  , testCase "Mutually recursive functions" $
      case parse "func isOdd : Nat -> Bool; isEven : Nat -> Bool where\n\
                 \  isOdd  zero     = false;\n\
                 \  isEven zero     = true;\n\
                 \  isOdd  (suc x)  = isEven x;\n\
                 \  isEven (suc x)  = isOdd x." of
       Right x -> x @?= (Program [ Func [("isOdd", Signature ["Nat", "Bool"])
                                        ,("isEven", Signature ["Nat", "Bool"])]
                                        [ Lambda "isOdd"
                                                 (Args [ExpId "zero"])
                                                 (ExpList [ExpId "false"])
                                        , Lambda "isEven"
                                                 (Args [ExpId "zero"])
                                                 (ExpList [ExpId "true"])
                                        , Lambda "isOdd"
                                                 (Args [ExpList [ExpId "suc", ExpId "x"]])
                                                 (ExpList [ExpId "isEven", ExpId "x"])
                                        , Lambda "isEven"
                                                 (Args [ExpList [ExpId "suc", ExpId "x"]])
                                                 (ExpList [ExpId "isOdd", ExpId "x"])]])
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
