{-|
Module:      Semantic.Ext
Description: Utilitary functions and constants for testing the DependentTypes.Semantic module.
-}

module Semantic.Ext where

import Control.Exception
import Data.IORef
import Data.Map (Map)
import DependentTypes.Data
import DependentTypes.Semantic (Env, evalProgram)
import Test.Tasty.HUnit hiding (assertEqual)
import qualified Test.Tasty.HUnit as HUnit (assertEqual)

assertEqual :: IORef (Map String Toplevel) -> IORef (Map String Toplevel) -> IO ()
assertEqual envRef1 envRef2 = do
  env1 <- readIORef envRef1
  env2 <- readIORef envRef2
  HUnit.assertEqual "" env1 env2

assertException :: Env -> Program -> IO ()
assertException env p = do
  except <- try $ evalProgram env p :: IO (Either ErrorCall ())
  case except of
   Right p -> assertFailure "Invalid program was accepted."
   Left  e -> return ()

nat :: Toplevel
nat = Type "Nat" (Signature ["Type"]) [ Constructor "zero" (Args []) (Signature ["Nat"])
                                        NoConstraint
                                      , Constructor "suc" (Args []) (Signature ["Nat", "Nat"])
                                        NoConstraint ]

bool :: Toplevel
bool = Type "Bool" (Signature ["Type"]) [ Constructor "false" (Args []) (Signature ["Bool"])
                                          NoConstraint
                                        , Constructor "true" (Args []) (Signature ["Bool"])
                                          NoConstraint ]
