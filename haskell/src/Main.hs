module Main where


import System.IO
import Control.Monad
import Parser
import Eval

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do 
   result <- prompt
   if pred result 
      then return ()
      else action result >> until_ pred prompt action
           

main :: IO ()
main = do
  _ <- putStrLn "Welcome to cek-scheme"
  until_ (== ":quit") (readPrompt ">>> ")  (putStrLn . show  . run)
  where run input = (tryParseExpr input) >>= evaluate
          
