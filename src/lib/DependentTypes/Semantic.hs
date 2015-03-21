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
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import DependentTypes.Data

-- | Mutable map with current program definitions.
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
evalToplevel env t@(Type name sig cons) = do
  checkTypeSignature env name sig
  modifyIORef env (Map.insert name t)
  forM_ cons $ \c@(Constructor consName _ sig _) -> do
                      checkConsSignature env consName sig
                      modifyIORef env (Map.insert consName t)
evalToplevel env f@(Func ss lambdas) = do
  checkFuncSignature env ss
  checkFuncLambdas env ss lambdas
  forM_ ss $ \s -> modifyIORef env (Map.insert (fst s) $ Func [s] lambdas)
evalToplevel env print@(Print exp) = do
  evalExp <- evalExpression env exp
  putStrLn $ printExpression evalExp ++ "."

-- | Checks if a signature for a type is valid.
checkTypeSignature :: Env -> String -> Signature -> IO ()
checkTypeSignature env name (Signature []) = error $ name ++ ": Type must have signature"
checkTypeSignature env name (Signature [s]) = do
  unless (s == "Type") $ error (name ++ ": " ++ "Type signature must end with 'Type'.")
checkTypeSignature env name (Signature (s:ss)) = do
  e <- readIORef env
  unless (s == "Type" || isValidType e s) $ error ((typeName s) ++ ": undefined type.")
  checkTypeSignature env name (Signature ss)

-- | Checks if a signature for a constructor is valid.
checkConsSignature :: Env -> String -> Signature -> IO ()
checkConsSignature env name (Signature []) = error $ name ++ ": Constructor must have signature"
checkConsSignature env name (Signature ss) = do
  forM_ ss $ \s -> do
    e <- readIORef env
    unless (isValidType e s) $ error ((typeName s) ++ ": undefined type.")

-- | Checks if a type name refers to an existing type with the same arity.
isValidType :: Map String Toplevel -> TypeDef -> Bool
isValidType m (TypeId name) = case Map.lookup name m of
                               Just (Type n (Signature [x]) _) -> n == name
                               _                               -> False
isValidType m (DepType name es) = case Map.lookup name m of
                                   Just (Type n (Signature ts) _) -> n == name && isTypeAssignable es ts
                                   _                              -> False

-- | Checks if a list of expressions is assignable to a list of types.
isTypeAssignable :: [Expression] -> [TypeDef] -> Bool
isTypeAssignable es ts = length es == length ts

-- | Checks if a signature for a function is valid.
checkFuncSignature :: Env -> [(String, Signature)] -> IO ()
checkFuncSignature env ss = forM_ ss $ \s -> checkFuncSignature' env s
  where
    checkFuncSignature' env (name, Signature []) = error $ name ++ ": " ++ "Function must have signature"
    checkFuncSignature' env (name, Signature ss) = do
      forM_ ss $ \s -> do
        e <- readIORef env
        unless (isValidType e s) $ error ((typeName s) ++ ": undefined type.")

-- | Checks if the definitions of a function are valid.
checkFuncLambdas :: Env -> [(String, Signature)] -> [Lambda] -> IO ()
checkFuncLambdas env ss [] = error $ ""
checkFuncLambdas env ss ls = forM_ ls $ \l -> checkFuncLambda env ss l
  where
    checkFuncLambda env ss l@(Lambda name args exp) = do
      checkLambdaArgs env name ss args
      checkLambdaBody env name ss args exp

-- | Checks if the arguments of a function definition is valid.
checkLambdaArgs :: Env -> String -> [(String, Signature)] -> Args -> IO ()
checkLambdaArgs env name ss (Args args) = do
  case find eqName ss of
   Just (_, (Signature s)) -> do
     unless (length s == 1 + length args) $ error (name ++ ": wrong number of arguments")
   Nothing -> error $ name ++ ": undefined function"
  where
    eqName (sigName, _) = sigName == name

checkLambdaBody :: Env -> String -> [(String, Signature)] -> Args -> Expression -> IO ()
checkLambdaBody env name ss args exp = return ()

-- | Evaluates an expression.
evalExpression :: Env -> Expression -> IO Expression
evalExpression env expId@(ExpId e) = do
  m <- readIORef env
  unless (isConstructor m e) $ error (e ++ ": " ++ "Expression could not be evaluated")
  return expId
evalExpression env (ExpList exps) = do
  liftM ExpList $ forM exps (evalExpression env)

-- | Checks if it is a valid constructor.
isConstructor :: Map String Toplevel -> String -> Bool
isConstructor m name = case Map.lookup name m of
                        Just (Type n _ _) -> n /= name
                        _                 -> False

-- | Prints an expression.
printExpression :: Expression -> String
printExpression (ExpId expId) = expId
printExpression (ExpList exps) = intercalate "; " $ map printExpression' exps
  where
    printExpression' (ExpId expId) = expId
    printExpression' (ExpList exps) = "(" ++ (unwords $ map printExpression' exps) ++ ")"
