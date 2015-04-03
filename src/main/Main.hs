module Main where

import Control.Exception
import Control.Monad
import Data.IORef
import DependentTypes.Data
import DependentTypes.Parser
import DependentTypes.Semantic
import System.Environment
import System.IO

main :: IO ()
main = do
  env <- nullEnv
  args <- getArgs
  case args of
   [] -> do
     putStrLn "dependent-types v0.1.0.0"
     putStrLn ""
     interpret env
   files -> forM_ files (execFile env)

interpret :: Env -> IO ()
interpret env = do
  cmd <- getCommand
  when (cmd /= "exit.") $ do
    case parse cmd of
     Right p -> eval env interpret p
     Left  e -> putParseError env e

getCommand :: IO String
getCommand = getInput ">>> " >>= getCommand'
  where
    getCommand' input = if any (=='.') input
                        then return input
                        else getInput "... " >>= \newInput -> getCommand' $ input ++ "\n" ++  newInput

getInput :: String -> IO String
getInput output = putStr output >> hFlush stdout >> getLine

execFile :: Env -> String -> IO ()
execFile env file = do
  input <- readFile file
  case parse input of
   Right p -> eval env (\e -> return ()) p
   Left  e -> putParseError env e

putParseError :: (Show a) => Env -> a -> IO ()
putParseError env e = (putStrLn $ show e) >> interpret env

eval :: Env -> (Env -> IO ()) -> Program -> IO ()
eval env interpret p = do
  envMap <- readIORef env
  newEnv <- newIORef envMap
  except <- try $ evalProgram putStrLn newEnv p :: IO (Either ErrorCall ())
  case except of
   Right () -> interpret newEnv
   Left  e  -> (putStrLn $ show e) >> interpret env
