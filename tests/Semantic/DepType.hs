module Semantic.DepType
       ( depTypeTests
       ) where

import DependentTypes.Data
import DependentTypes.Parser
import DependentTypes.Semantic
import Semantic.Ext hiding (assertEqual)
import Test.Tasty
import Test.Tasty.HUnit hiding (assertEqual)
import qualified Test.Tasty.HUnit as HUnit (assertEqual)

assertEqual :: String -> String -> IO ()
assertEqual = HUnit.assertEqual ""

bLessOrEqual = lessOrEqual ++
               "type Bounded : Type where\n\
               \  lowerBound : Bounded;\n\
               \  upperBound : Bounded;\n\
               \  bounded    : Nat -> Bounded.\n\
               \type BLessOrEqual : Bounded -> Bounded -> Type where\n\
               \  bLessLower lowerBound y : Bounded -> Bounded -> (BLessOrEqual lowerBound y);\n\
               \  bLessUpper x upperBound : Bounded -> Bounded -> (BLessOrEqual x upperBound);\n\
               \  bLessLift (bounded x) (bounded y) : Bounded -> Bounded ->\n\
               \    (BLessOrEqual (bounded x) (bounded y)) | LessOrEqual x y."

insert = oList ++
         "func insert : Nat -> (OList l u) -> (BLessOrEqual l (bounded x)) ->\n\
         \                (BLessOrEqual (bounded x) u) -> (OList l u) where\n\
         \  insert x (onil z) (BLessOrEqual l (bounded x)) (BLessOrEqual (bounded x) u) =\n\
         \    ocons x (onil (BLessOrEqual (bounded x) u)) (BLessOrEqual l (bounded x));\n\
         \  insert x (ocons y ys (BLessOrEqual l y)) (BLessOrEqual l (bounded x))\n\
         \      (BLessOrEqual (bounded x) u) =\n\
         \    matches (LessOrEqual x y)\n\
         \      (ocons x (ocons y ys (BLessOrEqual (bounded x) (bounded y))) (BLessOrEqual l (bounded x)))\n\
         \      (ocons y (insert x ys (BLessOrEqual (bounded y) (bounded x))\n\
         \             (BLessOrEqual (bounded x) u))\n\
         \             (BLessOrEqual l (bounded y)))."

lessOrEqual = "type LessOrEqual : Nat -> Nat -> Type where\n\
              \  lessZero zero    y       : Nat -> Nat -> (LessOrEqual zero y);\n\
              \  lessSuc  (suc x) (suc y) : Nat -> Nat -> (LessOrEqual (suc x) (suc y)) | LessOrEqual x y."

oList = bLessOrEqual ++
        "type OList : Bounded -> Bounded -> Type where\n\
        \  onil       : (BLessOrEqual l u) -> (OList l u);\n\
        \  ocons x xs : Nat -> (OList (bounded x) u) -> \n\
        \                 (BLessOrEqual l (bounded x)) -> (OList l u)."

pairAdd = "func add : Nat -> Nat -> Nat where\n\
          \  add zero    y    = y;\n\
          \  add x       zero = x;\n\
          \  add (suc x) y    = suc (add x y).\n\
          \func pairAdd : (List n Nat) -> (List n Nat) -> (List n Nat) where\n\
          \  pairAdd nil         nil         = nil;\n\
          \  pairAdd (cons x xs) (cons y ys) = cons (add x y) (pairAdd xs ys)."

sumOfOdd = "type Odd : Nat -> Type where\n\
           \  oddOne : (Odd (suc zero));\n\
           \  oddSuc : (Odd n) -> (Odd (suc (suc n))).\n\
           \type Even : Nat -> Type where\n\
           \  evenZero : (Even zero);\n\
           \  evenSuc  : (Even n) -> (Even (suc (suc n))).\n\
           \func add : Nat -> Nat -> Nat where\n\
           \  add zero    y    = y;\n\
           \  add x       zero = x;\n\
           \  add (suc x) y    = suc (add x y).\n\
           \func sumOfOdd : (Odd n) -> (Odd m) -> (Even (add n m)) where\n\
           \  sumOfOdd oddOne     oddOne     = evenSuc evenZero;\n\
           \  sumOfOdd oddOne     (oddSuc y) = evenSuc (sumOfOdd oddOne y);\n\
           \  sumOfOdd (oddSuc x) y          = evenSuc (sumOfOdd x y)."

depTypeTests = testGroup "Dependent Types"
  [ testCase "List type definition" $
    do
      env <- fromList naturals
      case parse "type List : Nat -> Type -> Type where\n\
                 \  nil  : (List zero a);\n\
                 \  cons : a -> (List n a) -> (List (suc n) a)." of
       Right p -> evalQuietProgram env p
       Left  e -> assertFailure $ show e

  , testCase "Printing empty list" $
    do
      env <- fromList lists
      case parse "print nil." of
       Right p -> evalProgram (assertEqual "nil.") env p
       Left  e -> assertFailure $ show e

  , testCase "Printing list" $
    do
      env <- fromList $ lists ++ booleans
      case parse "print (cons true (cons false nil))." of
       Right p -> evalProgram (assertEqual "(cons true (cons false nil)).") env p
       Left  e -> assertFailure $ show e

  , testCase "Printing list of different types" $
    do
      env <- fromList $ lists ++ booleans
      case parse "print (cons zero (cons false nil))." of
       Right p -> assertException env p
       Left  e -> assertFailure $ show e

  , testCase "length function definition" $
    do
      env <- fromList lists
      case parse "func length : (List n a) -> Nat where\n\
                 \  length list = n." of
       Right p -> evalQuietProgram env p
       Left  e -> assertFailure $ show e

  , testCase "Printing length call" $
    do
      env <- fromList $ lists ++ booleans
      case parse "func length : (List n a) -> Nat where\n\
                 \  length list = n.\n\
                 \print (length (cons true (cons false nil)))." of
       Right p -> evalProgram (assertEqual "(suc (suc zero)).") env p
       Left  e -> assertFailure $ show e

  , testCase "pairAdd function definition" $
    do
      env <- fromList lists
      case parse pairAdd of
       Right p -> evalQuietProgram env p
       Left  e -> assertFailure $ show e

  , testCase "Calling pairAdd with lists of wrong type" $
    do
      env <- fromList $ lists ++ booleans
      case parse (pairAdd ++
                  "print (pairAdd (cons false nil) (cons true nil)).") of
       Right p -> assertException env p
       Left  e -> assertFailure $ show e

  , testCase "Calling pairAdd with lists of different sizes" $
    do
      env <- fromList $ lists ++ booleans
      case parse (pairAdd ++
                  "print (pairAdd (cons zero (cons zero nil)) (cons zero nil)).") of
       Right p -> assertException env p
       Left  e -> assertFailure $ show e

  , testCase "Odd and Even type definitions" $
    do
      env <- fromList naturals
      case parse "type Odd : Nat -> Type where\n\
                 \  oddOne : (Odd (suc zero));\n\
                 \  oddSuc : (Odd n) -> (Odd (suc (suc n))).\n\
                 \type Even : Nat -> Type where\n\
                 \  evenZero : (Even zero);\n\
                 \  evenSuc  : (Even n) -> (Even (suc (suc n)))." of
       Right p -> evalQuietProgram env p
       Left  e -> assertFailure $ show e

  , testCase "Printing Odd number" $
    do
      env <- fromList naturals
      case parse "type Odd : Nat -> Type where\n\
                 \  oddOne : (Odd (suc zero));\n\
                 \  oddSuc : (Odd n) -> (Odd (suc (suc n))).\n\
                 \print oddOne; (oddSuc oddOne)." of
       Right p -> evalProgram (assertEqual "oddOne; (oddSuc oddOne).") env p
       Left  e -> assertFailure $ show e

  , testCase "sumOfOdd function definition" $
    do
      env <- fromList naturals
      case parse sumOfOdd of
       Right p -> evalQuietProgram env p
       Left  e -> assertFailure $ show e

  , testCase "Calling sumOfOdd" $
    do
      env <- fromList naturals
      case parse (sumOfOdd ++ "print (sumOfOdd (oddSuc oddOne) (oddSuc (oddSuc oddOne))).") of
       Right p -> evalProgram (assertEqual "(evenSuc (evenSuc (evenSuc (evenSuc evenZero)))).") env p
       Left  e -> assertFailure $ show e

  , testCase "Calling sumOfOdd with even number" $
    do
      env <- fromList naturals
      case parse (sumOfOdd ++ "print (sumOfOdd evenZero (evenSuc evenZero)).") of
       Right p -> assertException env p
       Left  e -> assertFailure $ show e

  , testCase "LessOrEqual type definition" $
    do
      env <- fromList naturals
      case parse lessOrEqual of
       Right p -> evalQuietProgram env p
       Left  e -> assertFailure $ show e

  , testCase "Printing LessOrEqual" $
    do
      env <- fromList naturals
      case parse (lessOrEqual ++ "print (lessZero zero (suc zero));\n\
                                 \(lessSuc (suc zero) (suc (suc zero))).") of
       Right p -> evalProgram
                    (assertEqual "(lessZero zero (suc zero)); (lessSuc (suc zero) (suc (suc zero))).")
                    env p
       Left  e -> assertFailure $ show e

  , testCase "Printing invalid LessOrEqual" $
    do
      env <- fromList naturals
      case parse (lessOrEqual ++ "print (lessSuc (suc (suc zero)) zero).") of
       Right p -> assertException env p
       Left  e -> assertFailure $ show e

  , testCase "BLessOrEqual type definition" $
    do
      env <- fromList naturals
      case parse bLessOrEqual of
       Right p -> evalQuietProgram env p
       Left  e -> assertFailure $ show e

  , testCase "Printing BLessOrEqual" $
    do
      env <- fromList naturals
      case parse (bLessOrEqual ++ "print (bLessLower lowerBound (bounded zero));\n\
                                  \ (bLessUpper (bounded zero) upperBound).") of
       Right p -> evalProgram (assertEqual "(bLessLower lowerBound (bounded zero)); \
                                           \(bLessUpper (bounded zero) upperBound).")
                              env p
       Left  e -> assertFailure $ show e

  , testCase "Printing invalid BLessOrEqual" $
    do
      env <- fromList naturals
      case parse (bLessOrEqual ++ "print (bLessLift (bounded (suc zero)) (bounded zero)).") of
       Right p -> assertException env p
       Left  e -> assertFailure $ show e

  , testCase "Ordered List type definition" $
    do
      env <- fromList naturals
      case parse oList of
       Right p -> evalQuietProgram env p
       Left  e -> assertFailure $ show e

  , testCase "toList function definition" $
    do
      env <- fromList lists
      case parse (oList ++ "func toList : (OList l u) -> (List n Nat) where\n\
                           \  toList (onil z)       = nil;\n\
                           \  toList (ocons x xs z) = cons x (toList xs).") of
       Right p -> evalQuietProgram env p
       Left  e -> assertFailure $ show e

  , testCase "Calling toList" $
    do
      env <- fromList lists
      case parse (oList ++ "func toList : (OList l u) -> (List n Nat) where\n\
                           \  toList (onil z)       = nil;\n\
                           \  toList (ocons x xs z) = cons x (toList xs).\n\
                           \print (toList (ocons zero (onil (bLessLower lowerBound upperBound))\n\
                           \                          (bLessLower lowerBound upperBound))).") of
       Right p -> evalProgram (assertEqual "(cons zero nil).") env p
       Left  e -> assertFailure $ show e

  , testCase "insert function definition" $
    do
      env <- fromList naturals
      case parse insert of
       Right p -> evalQuietProgram env p
       Left  e -> assertFailure $ show e

  , testCase "Calling insert" $
    do
      env <- fromList lists
      case parse (insert ++ "print (insert (suc zero)\n\
                            \              (insert zero (onil (bLessLower lowerBound upperBound))\n\
                            \                      (bLessLower lowerBound (bounded zero))\n\
                            \                      (bLessUpper (bounded zero) upper))\n\
                            \              (bLessLower lowerBound (bounded (suc zero)))\n\
                            \              (bLessUpper (bounded (suc zero)) upperBound)).") of
       Right p -> evalProgram (assertEqual "(cons zero (cons (suc zero) nil)).") env p
       Left  e -> assertFailure $ show e
  ]
