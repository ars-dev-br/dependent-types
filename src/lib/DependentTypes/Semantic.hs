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
import Data.Maybe
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
evalProgram :: (String -> IO ()) -> Env -> Program -> IO ()
evalProgram action env (Program ts) = forM_ ts $ evalToplevel action env

-- | Evaluates a toplevel construct.
evalToplevel :: (String -> IO ()) -> Env -> Toplevel -> IO ()
evalToplevel action env t@(Type name sig cons) = do
  checkTypeSignature env name sig
  modifyIORef env (Map.insert name t)
  forM_ cons $ \c@(Constructor consName _ sig _) -> do
                      checkConsSignature env consName sig
                      modifyIORef env (Map.insert consName t)
evalToplevel action env f@(Func ss lambdas) = do
  checkFuncSignature env ss
  forM_ ss $ \s@(name, _) -> modifyIORef env (Map.insert name $ Func [s] lambdas)
  checkFuncLambdas env ss lambdas
evalToplevel action env print@(Print exp) = do
  e <- readIORef env
  evalExp <- forM exp $ evalExpression e
  action $ showExpressions evalExp

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

-- | Checks if the body of a function uses an invalid symbol.  This may be both
-- that an undefined symbol is used or that a symbol is called with the wrong
-- number/type of arguments.
checkLambdaBody :: Env -> String -> [(String, Signature)] -> Args -> Expression -> IO ()
checkLambdaBody env name ss args exp@(ExpId expId) = do
  e <- readIORef env
  when (undefinedId e ss args expId) $ error (expId ++ ": undefined symbol")
  -- when (wrongArgs e ss args exp) $ error (expId ++ ": invalid arguments")
checkLambdaBody env name ss args exp@(ExpList expList) = do
  forM_ expList (checkLambdaBody env name ss args)
  e <- readIORef env
  case checkArgs e ss args exp of
   Right ()  -> return ()
   Left name -> error (name ++ ": invalid arguments")

-- | Checks if an id is undefined (i.e. it's neither a type, a constructor, a
-- function nor one argument).
undefinedId :: Map String Toplevel -> [(String, Signature)] -> Args -> String -> Bool
undefinedId e ss (Args args) expId = expId `Map.notMember` e &&
                                     all notEqExpId args &&
                                     all notEqExpId' ss
  where
    notEqExpId arg = case arg of
                      ExpId argExp   -> argExp /= expId
                      ExpList argExp -> all notEqExpId argExp

    notEqExpId' (sigName, _) = sigName /= expId

-- | Checks if a symbol is being called with the wrong number/type of arguments.
checkArgs :: Map String Toplevel -> [(String, Signature)] -> Args -> Expression -> Either String ()
checkArgs e ss (Args args) (ExpId expId) = do
  when (expId `Map.member` e) $ checkIdEnv e expId
  when (any eqExpId args)     $ checkIdArgs args expId
  when (any eqExpId' ss)      $ checkIdSigs ss expId
    where
      eqExpId arg = case arg of
                     ExpId argExp   -> argExp == expId
                     ExpList argExp -> any eqExpId argExp

      eqExpId' (sigName, _) = sigName == expId

checkArgs e ss args (ExpList [ExpId expId]) = checkArgs e ss args (ExpId expId)
checkArgs e ss args@(Args as) (ExpList expList@((ExpId expId):expTail)) = do
  forM_ expTail $ checkArgs e ss args
  when (expId `Map.member` e) $ checkCallEnv e expList
  when (any eqExpId ss)       $ checkCallSigs ss expList
    where
      eqExpId (sigName, _) = sigName == expId

-- | Checks if a symbol is being called with the wrong number/type of arguments
-- according to the environment.
checkIdEnv :: Map String Toplevel -> String -> Either String ()
checkIdEnv e expId = do
  case expId `Map.lookup` e of
   Just (Type name (Signature ss)  cons) | name == expId -> when (length ss /= 1) $ Left expId
                                         | otherwise     -> checkIdCons cons expId
   Just (Func [(_, (Signature ss))] _) -> when (length ss /= 1) $ Left expId
   Just (Var exp) -> Right ()
   Nothing -> Left expId

checkIdCons :: [Constructor] -> String -> Either String ()
checkIdCons cs expId = forM_ cs checkIdCons'
  where
    checkIdCons' (Constructor name _ (Signature sig) _) = do
      when (name == expId && length sig /= 1) $ Left expId

-- | Checks if a symbol is being called with the wrong number/type of arguments
-- according to the arguments of a function.
checkIdArgs :: [Expression] -> String -> Either String ()
checkIdArgs args expId = Right ()

-- | Checks if a symbol is being called with the wrong number/type of arguments
-- according to the signature of a function being defined.
checkIdSigs :: [(String, Signature)] -> String -> Either String ()
checkIdSigs ss expId = forM_ ss checkIdSigs'
  where
    checkIdSigs' (name, (Signature sig)) = do
      when (name == expId && length sig /= 1) $ Left expId

-- | Checks if a call is being made with the wrong number/type of arguments
-- according to the environment.
checkCallEnv :: Map String Toplevel -> [Expression] -> Either String ()
checkCallEnv e exp@((ExpId expId):_) = do
  case expId `Map.lookup` e of
   Just (Type name (Signature ss) cons) | name == expId -> when (length exp /= length ss) $ Left expId
                                        | otherwise     -> checkCallCons cons exp
   Just (Func [(_, (Signature ss))] _) -> when (not $ isTypeAssignable exp ss) $ Left expId
   Just (Var exp) -> Left expId
   Nothing -> Left expId

checkCallCons :: [Constructor] -> [Expression] -> Either String ()
checkCallCons cs exp@((ExpId expId):_) = forM_ cs checkCallCons'
  where
    checkCallCons' (Constructor name _ (Signature ss) _) = do
      when (name == expId && (not $ isTypeAssignable exp ss)) $ Left expId

-- | Checks if a call is being made with the wrong number/type of arguments
-- according to the signature of a function being defined.
checkCallSigs :: [(String, Signature)] -> [Expression] -> Either String ()
checkCallSigs ss exp@((ExpId expId):_) = forM_ ss checkCallSigs'
  where
    checkCallSigs' (name, (Signature ss)) = do
      when (name == expId && (not $ isTypeAssignable exp ss)) $ Left expId

-- | Evaluates an expression.
evalExpression :: Map String Toplevel -> Expression -> IO Expression
evalExpression e exp =
  case tryEvalExpression e exp of
   Right exp -> return exp
   Left err -> error err

-- | Tries to evaluate an expression
tryEvalExpression :: Map String Toplevel -> Expression -> Either String Expression
tryEvalExpression e exp@(ExpId expId) =
  if undefinedId e [] (Args []) expId
    then Left $ expId ++ ": undefined symbol"
    else case checkIdEnv e expId of
          Right () -> evalId e expId
          Left _   -> Left $ expId ++ ": invalid arguments"

tryEvalExpression e (ExpList [exp]) = tryEvalExpression e exp

tryEvalExpression e exp@(ExpList ((ExpId expId):expTail)) =
  case forM expTail $ tryEvalExpression e of
   Right expArgs -> case checkCallEnv e $  (ExpId expId):expArgs of
                     Right () -> evalList e $ (ExpId expId):expArgs
                     Left err -> Left $ err ++ ": invalid arguments"
   Left  name    -> Left name

-- | Evaluates a single symbol, such as a parameterless contructor or function.
evalId :: Map String Toplevel -> String -> Either String Expression
evalId e expId =
  case expId `Map.lookup` e of
   Just (Type _ _ _) -> Right (ExpId expId)
   Just (Func _ [l])  -> evalLambda e l expId
   Just (Var exp)    -> Right exp
   _                 -> Left $ expId ++ ": undefined symbol"
  where
    evalLambda e (Lambda _ _ exp) expId = tryEvalExpression e exp

evalList :: Map String Toplevel -> [Expression] -> Either String Expression
evalList e ((ExpId expId):expArgs) =
  case expId `Map.lookup` e of
   Just (Type _ _ _) -> Right (ExpList $ (ExpId expId):expArgs)
   Just (Func _ ls)  -> evalLambdas e ls
   _                 -> Left $ expId ++ ": undefined symbol"
  where
   evalLambdas e ls =
     case findLambda ls of
      Just (Lambda _ args exp) -> let env = bindVars e args expArgs
                                  in  tryEvalExpression env exp
      Nothing                  -> Left $ expId ++ ": no matching pattern found"

   findLambda ls = find findLambda' ls
   findLambda' (Lambda name (Args ((ExpId argId):_)) exp) =
     argId `Map.member` e && ExpId argId == head expArgs ||
     argId `Map.notMember` e && name == expId

bindVars :: Map String Toplevel -> Args -> [Expression] -> Map String Toplevel
bindVars e (Args []) [] = e
bindVars e (Args ((ExpId name):as)) (exp:exps) = Map.insert name (Var exp) $ bindVars e (Args as) exps

-- | Converts Expressions to a String.
showExpressions :: [Expression] -> String
showExpressions exps = (intercalate "; " $ map showExpression exps) ++ "."
  where
    showExpression (ExpId expId) = expId
    showExpression (ExpList exps) = "(" ++ (unwords $ map showExpression exps) ++ ")"
