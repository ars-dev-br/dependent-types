{-|
Module:      Semantic.Ext
Description: Utilitary functions and constants for testing the DependentTypes.Semantic module.
-}

module Semantic.Ext
       ( assertEqual
       , assertException
       , booleans
       , evalQuietProgram
       , lists
       , naturals
       ) where

import Control.Exception
import Data.IORef
import Data.Map (Map)
import DependentTypes.Data
import DependentTypes.Semantic (Env, evalProgram)
import Test.Tasty.HUnit hiding (assertEqual)
import qualified Test.Tasty.HUnit as HUnit (assertEqual)

-- | Asserts that two environments are equal.
assertEqual :: Env -> Env -> IO ()
assertEqual envRef1 envRef2 = do
  env1 <- readIORef envRef1
  env2 <- readIORef envRef2
  HUnit.assertEqual "" env1 env2

-- | Evaluates a Program, making sure it does not print anything.
evalQuietProgram :: Env -> Program -> IO ()
evalQuietProgram env p = evalProgram (assertFailure . ("unexpected output: "++)) env p

-- | Asserts that an exception of type ErrorCall was thrown.
assertException :: Env -> Program -> IO ()
assertException env p = do
  except <- try $ evalQuietProgram env p :: IO (Either ErrorCall ())
  case except of
   Right p -> assertFailure "Invalid program was accepted."
   Left  e -> return ()

-- | Type definition for the natural numbers.
naturals :: [(String, Toplevel)]
naturals = [("Nat", nat), ("zero", nat), ("suc", nat)]

nat :: Toplevel
nat = Type "Nat" (Signature ["Type"]) [ Constructor "zero" (Args []) (Signature ["Nat"])
                                        NoConstraint
                                      , Constructor "suc" (Args []) (Signature ["Nat", "Nat"])
                                        NoConstraint ]

-- | Type definition for the booleans.
booleans :: [(String, Toplevel)]
booleans = [("Bool", bool), ("true", bool), ("false", bool)]

bool :: Toplevel
bool = Type "Bool" (Signature ["Type"]) [ Constructor "false" (Args []) (Signature ["Bool"])
                                          NoConstraint
                                        , Constructor "true" (Args []) (Signature ["Bool"])
                                          NoConstraint ]

-- | Type definition for the List
lists :: [(String, Toplevel)]
lists = [("List", depLists), ("nil", depLists), ("cons", depLists)] ++ naturals

depLists :: Toplevel
depLists = Type "List" (Signature ["Nat", "Type", "Type"])
           [ Constructor "nil" (Args []) (Signature [DepType "List" [ExpId "zero", ExpId "a"]])
             NoConstraint
           , Constructor "cons" (Args []) (Signature [ TypeId "a"
                                                     , DepType "List" [ExpId "n", ExpId "a"]
                                                     , DepType "List" [ ExpList [ ExpId "suc"
                                                                                , ExpId "n" ]
                                                                      , ExpId "a" ]])
             NoConstraint]
