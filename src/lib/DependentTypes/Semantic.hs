{-|
Module:      DependentTypes.Semantic
Description: Semantic analysis and program interpreter.
-}

module DependentTypes.Semantic
       ( nullEnv
       , evalProgram
       , fromList
       ) where

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
evalToplevel env t@(Type name _ _)       = modifyIORef env (Map.insert name t)
evalToplevel env   (Func []     lambdas) = return ()
evalToplevel env f@(Func (s:ss) lambdas) = do
  modifyIORef env (Map.insert (fst s) $ Func [s] lambdas)
  evalToplevel env $ Func ss lambdas
evalToplevel env print@(Print exp) = do
  return ()
