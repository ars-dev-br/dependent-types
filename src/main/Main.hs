module Main where

import Control.Exception
import Control.Monad
import Data.IORef
import DependentTypes.Parser
import DependentTypes.Semantic
import System.IO

main :: IO ()
main = do
  env <- nullEnv
  putStrLn "dependent-types v0.1.0.0"
  putStrLn ""
  interpret env

interpret :: Env -> IO ()
interpret env = do
  cmd <- getCommand
  when (cmd /= "exit.") $ do
    case parse cmd of
     Right p -> eval p
     Left  e -> putParseError e
    where
      putParseError e = (putStrLn $ show e) >> interpret env
      eval p = do
        envMap <- readIORef env
        newEnv <- newIORef envMap
        except <- try $ evalProgram putStrLn newEnv p :: IO (Either ErrorCall ())
        case except of
         Right () -> interpret newEnv
         Left  e  -> (putStrLn $ show e) >> interpret env


getCommand :: IO String
getCommand = getInput ">>> " >>= getCommand'
  where
    getCommand' input = if any (=='.') input
                        then return input
                        else getInput "... " >>= \newInput -> getCommand' $ input ++ newInput

getInput :: String -> IO String
getInput output = putStr output >> hFlush stdout >> getLine
