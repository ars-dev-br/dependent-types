{-|
Module:      DependentTypes.Semantic
Description: Semantic analysis and program interpreter.
-}

module DependentTypes.Semantic
       ( Env
       , evalProgram
       , fromList
       , nullEnv
       ) where

import Control.Exception
import Control.Monad
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as Map
import DependentTypes.Data

type Env = IORef (Map String Toplevel)

-- | Creates an empty environment
nullEnv :: IO Env
nullEnv = newIORef Map.empty

-- | Creates an environment from a list
fromList :: [(String, Toplevel)] -> IO Env
fromList = newIORef . Map.fromList

-- | Evaluates a gramatically valid program.
evalProgram :: Env -> Program -> IO ()
evalProgram env (Program t) = evalToplevels env t

-- | Evaluates a list of toplevel constructs in order.
evalToplevels :: Env -> [Toplevel] -> IO ()
evalToplevels env []     = return ()
evalToplevels env (t:ts) = do
  evalToplevel env t
  evalToplevels env ts

-- | Evaluates a toplevel construct.
evalToplevel :: Env -> Toplevel -> IO ()
evalToplevel env t@(Type name sig cons)       = do
  checkTypeSignature env name sig
  modifyIORef env (Map.insert name t)
evalToplevel env   (Func []     lambdas) = return ()
evalToplevel env f@(Func (s:ss) lambdas) = do
  modifyIORef env (Map.insert (fst s) $ Func [s] lambdas)
  evalToplevel env $ Func ss lambdas
evalToplevel env print@(Print exp) = do
  return ()

-- | Checks if a signature for a type is valid
checkTypeSignature :: Env -> String -> Signature -> IO ()
checkTypeSignature env name (Signature []) = error $ name ++ ": " ++ "Type must have signature"
checkTypeSignature env name (Signature [s]) = do
  unless (s == "Type") $ error (name ++ ": " ++ "Type signature must end with 'Type'.")
checkTypeSignature env name (Signature (s:ss)) = do
  e <- readIORef env
  unless (s == "Type" || isType e s) $
    error (name ++ ": " ++ "Type signature must have already defined types.")
  checkTypeSignature env name (Signature ss)

-- | Checks if a type name refers to an existing type.
isType :: Map String Toplevel -> TypeDef -> Bool
isType m (TypeId name) = case Map.lookup name m of
                          Just (Type _ _ _) -> True
                          _                 -> False
isType m (DepType name _) = case Map.lookup name m of
                             Just (Type _ _ _) -> True
                             _                 -> False
