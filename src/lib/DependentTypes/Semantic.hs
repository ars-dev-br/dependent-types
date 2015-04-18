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
import Data.Char
import Data.IORef
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Debug.Trace
import DependentTypes.Data

-- | Mutable map with current program definitions.
type Env = IORef (Map String Toplevel)

-- | Predefined keywords.
keywords = ["match"]

-- | Creates an empty environment.
nullEnv :: IO Env
nullEnv = newIORef Map.empty

-- | Creates an environment from a list.
fromList :: [(String, Toplevel)] -> IO Env
fromList = newIORef . Map.fromList

-- | Evaluates a gramatically valid program.
evalProgram :: (String -> IO ()) -> Env -> Program -> IO ()
evalProgram action env (Program ts) = forM_ ts $ evalToplevel action env

-- | Evaluates a toplevel construct.
evalToplevel :: (String -> IO ()) -> Env -> Toplevel -> IO ()
evalToplevel action env t@(Type name sig cons) = do
  checkTypeSignature env name sig
  modifyIORef env (name `Map.insert` t)
  forM_ cons $ \c@(Constructor consName _ sig _) -> do
                      checkConsSignature env consName sig
                      modifyIORef env (consName `Map.insert` t)
evalToplevel action env f@(Func ss lambdas) = do
  checkFuncSignature env ss
  forM_ ss $ \s@(name, _) -> modifyIORef env (name `Map.insert` Func [s] lambdas)
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
isValidType m (TypeId name) = case name `Map.lookup` m of
                               Just (Type n (Signature [x]) _) -> n == name
                               _                               -> isAsciiLower (head name)
isValidType m (DepType name es) = case name `Map.lookup` m of
                                   Just (Type n (Signature ts) _) -> n == name && isDepTypeAssignable m es ts
                                   _                              -> isAsciiLower (head name)

-- | Checks if a list of expressions is assignable to a list of types.
isTypeAssignable :: Map String Toplevel -> [Expression] -> [TypeDef] -> Bool
isTypeAssignable m [] [] = False
isTypeAssignable m [ExpId expId] [TypeId typeId] =
  case expId `Map.lookup` m of
   Just (Type typeName _ cons)         -> typeName == typeId || isAsciiLower (head typeId)
   Just (Func [(_, (Signature ss))] _) -> last ss == TypeId typeId
   Just (Var _)                        -> True
   Nothing                             -> True
isTypeAssignable m [ExpId expId] [DepType typeId _] =
  case expId `Map.lookup` m of
   Just (Type typeName _ cons)         -> typeName == typeId
   Just (Func [(_, (Signature ss))] _) -> isValidSignature (last ss)
   Just (Var _)                        -> True
   Nothing                             -> True
  where
    isValidSignature (DepType depType _) = depType == typeId
    isValidSignature (TypeId typeName)   = typeName == typeId
isTypeAssignable m (expHead@(ExpId expId):expTail) ts =
  1 + length expTail == length ts &&
  isTypeAssignable m [expHead] [(last ts)] &&
  (all (==True) $ zipWith isTypeAssignable' expTail ts)
  where
    isTypeAssignable' exp@(ExpId expId) t = isTypeAssignable m [exp] [t]
    isTypeAssignable' (ExpList (expHead:expTail)) t = isTypeAssignable m [expHead] [t]

-- | Checks if a list of expressions is assignable to a list of dependent types.  This is basically
-- the same as isTypeAssignable, but more lenient about undefined symbols.
isDepTypeAssignable :: Map String Toplevel -> [Expression] -> [TypeDef] -> Bool
isDepTypeAssignable m [] [] = False
isDepTypeAssignable m [ExpId expId] ((TypeId typeId):ts) =
  case expId `Map.lookup` m of
   Just (Type typeName _ cons)         -> typeName == typeId || (expId == typeName && typeId == "Type")
   Just (Func [(_, (Signature ss))] _) -> last ss == TypeId typeId
   Just (Var _)                        -> True
   Nothing                             -> True
isDepTypeAssignable m [ExpList expList] ts = isTypeAssignable m [head expList] (init ts)
isDepTypeAssignable m es ts = 1 + length es == length ts &&
                              (all (==True) $ zipWith isDepTypeAssignable' es (init ts))
  where
    isDepTypeAssignable' exp@(ExpId expId) t = isDepTypeAssignable m [exp] [t]
    isDepTypeAssignable' (ExpList (expHead:expTail)) t = isDepTypeAssignable m [expHead] [t]

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
  forM_ ss $ \(sigName, Signature ts) -> do
              when (sigName == name && (not $ isTypeAssignable e [exp] [last ts])) $
                error (expId ++ ": invalid type")
checkLambdaBody env name ss args exp@(ExpList expList) = do
  forM_ expList (checkLambdaBody' env name ss args)
  e <- readIORef env
  case checkArgs e ss args exp of
   Right ()  -> return ()
   Left name -> error (name ++ ": invalid arguments")
  where
    checkLambdaBody' env name ss args exp@(ExpId _) = checkLambdaBody env name ss args exp
    checkLambdaBody' env name ss args exp@(ExpList expList) = do
      e <- readIORef env
      case checkArgs e ss args exp of
       Right () -> return ()
       Left name -> error (name ++ ": invalid arguments")

-- | Checks if an id is undefined (i.e. it's neither a type, a constructor, a
-- function nor one argument).
undefinedId :: Map String Toplevel -> [(String, Signature)] -> Args -> String -> Bool
undefinedId e ss (Args args) expId = expId `notElem` keywords &&
                                     expId `Map.notMember` e &&
                                     all notEqExpId args &&
                                     all notEqExpId' ss &&
                                     all notInfersExpId ss
  where
    notEqExpId arg = case arg of
                      ExpId argExp   -> argExp /= expId
                      ExpList argExp -> all notEqExpId argExp

    notEqExpId' (sigName, _) = sigName /= expId

    notInfersExpId (_, Signature ss) = all notInfersExpId' ss

    notInfersExpId' (TypeId typeId) = typeId /= expId
    notInfersExpId' (DepType _ exp) = all notEqExpId exp

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
  when (any eqExpId ss)       $ checkCallSigs e ss expList
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

-- | Checks if a symbol is being called with the wrong number/type of arguments
-- according to a type constructor.
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
checkCallEnv e exp@((ExpId expId):_) =
  if expId `elem` keywords
  then Right ()
  else case expId `Map.lookup` e of
        Just (Type name (Signature ss) cons) -> checkCallCons e cons exp
        Just (Func [(_, (Signature ss))] _) -> when (not $ isTypeAssignable e exp ss) $ Left expId
        Just (Var exp) -> Left expId
        Nothing -> Left expId
checkCallEnv e ((ExpList expList):[]) = checkCallEnv e expList

-- | Checks if a call is being made with the wrong number/type of arguments
-- according to a type constructor
checkCallCons :: Map String Toplevel -> [Constructor] -> [Expression] -> Either String ()
checkCallCons e cs exp@((ExpId expId):_) = forM_ cs checkCallCons'
  where
    checkCallCons' (Constructor name _ (Signature ss) constraint) | name == expId = do
      checkInferredTypes e exp
      checkConstraint e exp constraint
      when (name == expId && (not $ isTypeAssignable e exp ss)) $ Left expId
                                                                  | otherwise = return ()

-- | Checks if a constructor call obeys a constraint.
checkConstraint :: Map String Toplevel -> [Expression] -> Constraint -> Either String ()
checkConstraint e es NoConstraint = return ()
checkConstraint e es@(expHead:expArgs) (Constraint (ExpList c@((ExpId cId):cs))) = do
  (Type _ _ cs)  <- lookupType e
  checkConstructors e cs
  where
    lookupType e = case cId `Map.lookup` e of
                    Just t@(Type name _ _) | name == cId -> return t
                                           | otherwise -> Left $ cId ++ " constraint"
                    Nothing -> Left $ cId ++ " constraint"

    checkConstructors e cs =
      case findConstructor cs of
       Just (Constructor name args sigs constraint) -> checkConstructor e name es args constraint
       Nothing -> Left $ cId ++ " constraint"

    findConstructor cs = find findConstructor' cs
    findConstructor' (Constructor _ (Args args) _ _) =
      all (==True) $ zipWith (match e) args expArgs

    checkConstructor e name ((ExpId expId):expTail) args NoConstraint = return ()
    checkConstructor e name ((ExpId expId):expTail) args c@(Constraint (ExpList (_:expArgs))) = do
      case tryEvalExpression (bindVars e args expTail) (ExpList ((ExpId name):expArgs)) of
       Right _ -> return ()
       Left  _ -> Left $ cId ++ " constraint"
    checkConstructor e name ((ExpList expList):_) args c = checkConstructor e name expList args c

-- | Checks if an expression pattern matches another.
match :: Map String Toplevel -> Expression -> Expression -> Bool
match e (ExpId arg) (ExpId expArg) = (arg `memberAndNotVar` e &&
                                     arg == expArg) ||
                                     (not $ arg `memberAndNotVar` e)
match e (ExpId arg) (ExpList expList) = not $ arg `memberAndNotVar` e
match e (ExpList (argId:_)) (ExpList (expId@(ExpId exp):_)) =
  argId == expId ||
  case exp `Map.lookup` e of
   Just (Type typeName _ _) -> ExpId typeName == argId
   _ -> False
match e (ExpList _) (ExpId _) = False

-- | Checks if the inferred types for type variables are valid.
checkInferredTypes :: Map String Toplevel -> [Expression] -> Either String (Map String Toplevel)
checkInferredTypes env exp = checkInferredTypes' Map.empty exp
  where
    checkInferredTypes' binds [ExpId expId] = return binds
    checkInferredTypes' binds exp@((ExpId expId):expTail) = do
      newBinds <- updatingForM binds expTail checkInferredTypes''
      case expId `Map.lookup` env of
       Just (Type _ _ cons) -> checkInferredCons newBinds cons exp
       Just (Func sigs _) -> checkInferredFunc newBinds sigs exp

    updatingForM binds [] fn = return binds
    updatingForM binds (e:es) fn = case fn binds e of
                                    Right newBinds -> updatingForM newBinds es fn
                                    Left  e        -> Left e

    checkInferredTypes'' binds (ExpId expId) = return binds
    checkInferredTypes'' binds (ExpList expList) = checkInferredTypes' binds expList

    checkInferredCons binds cons exp@((ExpId expId):expTail) =
      case find (\(Constructor name _ _ _) -> name == expId) cons of
       Just c -> checkInferredCons' binds c exp
       _      -> return binds -- types do not have inferred type variables, only constructors

    checkInferredCons' binds (Constructor _ _ sig _) exp = checkInferredSig binds sig exp

    checkInferredFunc binds sigs exp@((ExpId expId):expTail) = do
      case find (\(name, _) -> name == expId) sigs of
       Just (_, sig) -> checkInferredSig binds sig exp
       _             -> Left expId

    checkInferredSig binds (Signature sig) ((ExpId expId):expTail) =
      case updatingForM binds (zip sig expTail) bindTypeVars of
       Right newBinds -> return newBinds
       Left _         -> Left expId

    bindTypeVars binds (TypeId typeId, ExpId expId) = do
      case typeId `Map.lookup` binds of
       Just (Var (ExpId expBind)) -> if (expId `Map.lookup` env) == (expBind `Map.lookup` env)
                                     then return binds
                                     else Left expId
       _                          -> if isAsciiLower $ head typeId
                                     then return (typeId `Map.insert` (Var (ExpId expId)) $ binds)
                                     else return binds
    bindTypeVars binds (t, e) = return binds

-- | Checks if a call is being made with the wrong number/type of arguments
-- according to the signature of a function being defined.
checkCallSigs :: Map String Toplevel -> [(String, Signature)] -> [Expression] -> Either String ()
checkCallSigs e ss exp@((ExpId expId):_) = forM_ ss checkCallSigs'
  where
    checkCallSigs' (name, (Signature ss)) =
      when (name == expId && (not $ isTypeAssignable e exp ss)) $ Left expId

-- | Evaluates an expression.
evalExpression :: Map String Toplevel -> Expression -> IO Expression
evalExpression e exp =
  case tryEvalExpression e exp of
   Right exp -> return exp
   Left err  -> error err

-- | Tries to evaluate an expression.
tryEvalExpression :: Map String Toplevel -> Expression -> Either String Expression
tryEvalExpression e exp@(ExpId expId) =
  if undefinedId e [] (Args []) expId
    then Left $ expId ++ ": undefined symbol"
    else case checkIdEnv e expId of
          Right () -> evalId e expId
          Left _   -> Left $ expId ++ ": invalid arguments"
tryEvalExpression e (ExpList [exp]) = tryEvalExpression e exp
tryEvalExpression e exp@(ExpList ((ExpId expId):expTail)) =
  if expId `elem` keywords
  then tryEvalKeyword e exp
  else case forM expTail $ tryEvalExpression e of
        Right expArgs -> case checkCallEnv e $ (ExpId expId):expArgs of
                          Right () -> evalList e $ (ExpId expId):expArgs
                          Left err -> Left $ err ++ ": invalid arguments"
        Left  name    -> Left name

-- | Tries to evaluate a keyword expression.
tryEvalKeyword :: Map String Toplevel -> Expression -> Either String Expression
tryEvalKeyword e exp@(ExpList ((ExpId "match"):cond:ifTrue:ifFalse:[])) = do
  constraint <- constraintForType cond
  (ExpList newExp) <- tryEvalExpression e cond
  case checkConstraint e newExp constraint of
   Right _ -> tryEvalExpression e ifTrue
   Left  _ -> tryEvalExpression e ifFalse
  where
    constraintForType cond = case findConstructor cond of
                              Just (Constructor _ _ _ constraint) -> return constraint
                              _ -> return NoConstraint

    findConstructor (ExpList ((ExpId expId):expArgs)) =
      let (Type _ _ cs) = fromJust $ expId `Map.lookup` e
       in findConstructor' cs expArgs

    findConstructor' cs expArgs = find (findConstructor'' expArgs) cs
    findConstructor'' expArgs (Constructor _ (Args args) _ _) =
      all (==True) $ zipWith (match e) args expArgs

tryEvalKeyword e (ExpId expId) = Left $ expId ++ ": invalid use of keyword."
tryEvalKeyword e (ExpList (expHead:_)) = tryEvalKeyword e expHead

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

-- | Evaluates a list of expressions, such as a function call.
evalList :: Map String Toplevel -> [Expression] -> Either String Expression
evalList e ((ExpId expId):expArgs) =
  case expId `Map.lookup` e of
   Just (Type _ _ _) -> Right (ExpList $ (ExpId expId):expArgs)
   Just (Func _ ls)  -> evalLambdas e ls
   _                 -> Left $ expId ++ ": undefined symbol"
  where
   evalLambdas e ls =
     case findLambda ls of
      Just (Lambda _ args exp) -> tryEvalExpression (bindVars e args expArgs) exp
      Nothing                  -> Left $ expId ++ ": no matching pattern found"

   findLambda ls = find findLambda' ls
   findLambda' (Lambda name (Args args) _) =
     (name == expId) && (all (==True) $ zipWith (match e) args expArgs)

-- | Binds values to their respective variables.
bindVars :: Map String Toplevel -> Args -> [Expression] -> Map String Toplevel
bindVars e (Args []) [] = e
bindVars e (Args ((ExpId name):as)) (exp:exps) =
  if name `memberAndNotVar` e
  then bindVars e (Args as) exps
  else name `Map.insert` Var exp $ bindVars e (Args as) exps
bindVars e (Args ((ExpList expList):as)) ((ExpList exp):exps) =
  bindVars e (Args $ tail expList) (tail exp) `Map.union` bindVars e (Args as) exps

-- | Checks if a symbol is defined in the environment, but it's not a variable.
memberAndNotVar :: String -> Map String Toplevel -> Bool
memberAndNotVar arg e = case arg `Map.lookup` e of
                         Just (Var _)      -> False
                         Just (Func _ _)   -> True
                         Just (Type _ _ _) -> True
                         Nothing           -> False

-- | Converts Expressions to a String.
showExpressions :: [Expression] -> String
showExpressions exps = (intercalate "; " $ map showExpression exps) ++ "."
  where
    showExpression (ExpId expId) = expId
    showExpression (ExpList exps) = "(" ++ (unwords $ map showExpression exps) ++ ")"
